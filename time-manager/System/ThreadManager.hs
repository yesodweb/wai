{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A thread manager including a time manager.
--   The manager has responsibility to kill managed threads.
--
-- Because this is based on the accompanying "System.TimeManager" module,
-- the same caveats apply:
--
--   * Only works for GHC.
--   * Only works with a threaded runtime.
--   * Users of older versions should check the current semantics.
--   * Using 32-bit systems means the max timeout is @'maxBound' :: Int@
--     (2147483647) microseconds, which is less than 36 minutes.
--   * Using the same 'Handle' in different threads might cause issues in some
--     edge cases. (i.e. using cancel/pause in one thread, and resume in another)
module System.ThreadManager (
    ThreadManager,
    newThreadManager,
    stopAfter,
    stopAfterWithResult,
    KilledByThreadManager (..),

    -- * Fork
    Reservation,
    reserveManaged,
    takeReservation,
    forkManaged,
    forkManagedFinally,
    forkManagedUnmask,
    forkManagedTimeout,
    forkManagedTimeoutFinally,

    -- * Synchronization
    waitUntilAllGone,
    isAllGone,
    countManaged,

    -- * Re-exports
    T.Manager,
    withHandle,
    T.Handle,
    T.tickle,
    T.pause,
    T.resume,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (Exception (..), SomeException (..))
import qualified Control.Exception as E
import Control.Monad (unless, void)
import Data.Foldable (forM_)
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word64)
import GHC.Conc.Sync (labelThread)
#if __GLASGOW_HASKELL__ >= 908
import GHC.Conc.Sync (fromThreadId)
#endif
import System.Mem.Weak (Weak, deRefWeak)
import qualified System.TimeManager as T

----------------------------------------------------------------

-- | Manager to manage the thread and the timer.
data ThreadManager
    = ThreadManager T.Manager (TVar ManagedThreads) (TVar Int)

type Key = Word64
type ManagedThreads = Map Key ManagedThread

----------------------------------------------------------------

-- 'IORef' prevents race between WAI TimeManager (TimeoutThread)
-- and stopAfter (KilledByThreadManager).
-- It is initialized with 'False' and turned into 'True' when locked.
-- The winner can throw an asynchronous exception.
data ManagedThread = ManagedThread (Weak ThreadId) (IORef Bool)

----------------------------------------------------------------

-- | Create a thread manager.
--
-- To create a 'ThreadManager', you will first have to create a
-- 'T.Manager' from the "System.TimeManager" module.
--
-- You can use either 'System.TimeManager.initialize' or 'System.TimeManager.withManager'.
newThreadManager :: T.Manager -> IO ThreadManager
newThreadManager timmgr =
    ThreadManager timmgr <$> newTVarIO Map.empty <*> newTVarIO 0

----------------------------------------------------------------

-- | A place kept for a thread that is not running yet.
--
--   A caller which forks by itself cannot register the thread before it
--   starts -- only a thread can name itself -- so until it does, what is
--   counted is the place kept for it.
newtype Reservation = Reservation (TVar Int, IORef Bool)

-- | Keeping a place for a thread about to be forked.  It counts towards
--   'countManaged' from here, so that a manager stopped in between does
--   not see a connection that has been accepted as one that is gone.
reserveManaged :: ThreadManager -> IO Reservation
reserveManaged (ThreadManager _timmgr _var rvar) = do
    atomically $ modifyTVar' rvar (+ 1)
    ref <- newIORef False
    return $ Reservation (rvar, ref)

-- | Running the action of a thread which was reserved for, taking the
--   place kept for it: the thread records itself, so that 'stopAfter' can
--   reach it, and gives the reservation back.
--
--   The reservation is given back only once the thread is recorded, so
--   that it is counted throughout.
takeReservation :: ThreadManager -> Reservation -> IO () -> IO ()
takeReservation (ThreadManager _timmgr var _rvar) rsv io =
    E.handle ignore $
        E.bracket (setup var) (clear var) $ \_ -> do
            release rsv
            io
  where
    release (Reservation (rvar, ref)) = do
        taken <- atomicModifyIORef' ref (\b -> (True, b))
        unless taken $ atomically $ modifyTVar' rvar (subtract 1)

----------------------------------------------------------------

-- | An exception used internally to kill a managed thread.
data KilledByThreadManager = KilledByThreadManager (Maybe SomeException)
    deriving (Show)

instance Exception KilledByThreadManager where
    toException = E.asyncExceptionToException
    fromException = E.asyncExceptionFromException

-- | Stopping the manager.
--
-- @
-- stopAfter threadManager action cleanup
-- @
--
-- The action is run in the scope of an exception handler that catches all
-- exceptions (including asynchronous ones); this allows the cleanup handler
-- to cleanup in all circumstances. If an exception is caught, it is rethrown
-- after the cleanup is complete.
stopAfter :: ThreadManager -> IO a -> (Maybe SomeException -> IO ()) -> IO a
stopAfter mgr action cleanup =
    stopAfterWithResult mgr action $ \mResult ->
        case mResult of
            Left err -> do
                cleanup (Just err)
                E.throwIO err
            Right result -> do
                cleanup Nothing
                return result

-- | Generalization of 'stopAfter' where the cleanup handler is allowed to construct the final result
--
-- Unlike in 'stopAfter', if an exception is thrown, it is not re-thrown after the cleanup
-- handler completes; the cleanup handler itself can decide to rethrow it or compute a result.
--
-- @since 0.3.2
stopAfterWithResult
    :: ThreadManager -> IO a -> (Either SomeException a -> IO b) -> IO b
stopAfterWithResult (ThreadManager _timmgr var _rvar) action cleanup = do
    E.mask $ \unmask -> do
        ma <- E.try $ unmask action
        m <- atomically $ do
            m0 <- readTVar var
            writeTVar var Map.empty
            return m0
        let ths = Map.elems m
            er = either Just (const Nothing) ma
            ex = KilledByThreadManager er
        forM_ ths $ \(ManagedThread wtid ref) -> lockAndKill wtid ref ex
        cleanup ma

----------------------------------------------------------------

-- | Fork a managed thread.
--
-- The thread adds itself to the manager's queue as the first thing it
-- does, and is removed again when it terminates (normally or abnormally).
-- This call does not wait for that, so until the thread is scheduled it is
-- forked and not yet managed.  A caller which cannot afford that window
-- can keep a place with 'reserveManaged' instead.
forkManaged
    :: ThreadManager
    -> String
    -- ^ Thread name
    -> IO ()
    -- ^ Action
    -> IO ()
forkManaged mgr label io =
    forkManagedUnmask mgr label $ \unmask -> unmask io

-- | Like 'forkManaged', but run action with exceptions masked
forkManagedUnmask
    :: ThreadManager
    -> String
    -- ^ Thread name
    -> ((forall x. IO x -> IO x) -> IO ())
    -- ^ Action with unmask argument
    -> IO ()
forkManagedUnmask (ThreadManager _timmgr var _rvar) label io =
    void $ E.mask_ $ forkIOWithUnmask $ \unmask -> E.handle ignore $ do
        labelMe label
        E.bracket (setup var) (clear var) $ \_ -> io unmask

-- | Fork a managed thread with a handle created by a timeout manager.
forkManagedTimeout
    :: ThreadManager
    -> String
    -- ^ Thread name
    -> (T.Handle -> IO ())
    -- ^ Action with timeout handle
    -> IO ()
forkManagedTimeout (ThreadManager timmgr var _rvar) label io =
    void $ forkIO $ do
        labelMe label
        E.bracket (setup var) (clear var) $ \(_n, wtid, ref) ->
            E.handle ignore $ T.withHandle timmgr (lockAndKill wtid ref ex) io
  where
    ex = KilledByThreadManager Nothing

-- | Fork a managed thread with a cleanup function.
forkManagedFinally
    :: ThreadManager
    -> String
    -- ^ Thread name
    -> IO ()
    -- ^ Action
    -> IO ()
    -- ^ Cleanup function
    -> IO ()
forkManagedFinally mgr label io final =
    forkManagedUnmask mgr label $ \restore ->
        E.try (restore io) >>= \(_ :: Either E.SomeException ()) -> final

-- | Fork a managed thread with a handle created by a timeout manager
-- and with a cleanup function.
forkManagedTimeoutFinally
    :: ThreadManager
    -> String
    -- ^ Thread name
    -> (T.Handle -> IO ())
    -- ^ Action with timeout handle
    -> IO ()
    -- ^ Cleanup function
    -> IO ()
forkManagedTimeoutFinally mgr label io final = E.mask $ \restore ->
    forkManagedTimeout mgr label $ \th ->
        E.try (restore $ io th) >>= \(_ :: Either E.SomeException ()) -> final

setup :: TVar (Map Key ManagedThread) -> IO (Key, Weak ThreadId, IORef Bool)
setup var = do
    (wtid, n) <- myWeakThreadId
    ref <- newIORef False
    let ent = ManagedThread wtid ref
    -- asking to throw KilledByThreadManager to me
    atomically $ modifyTVar' var $ Map.insert n ent
    return (n, wtid, ref)

lockAndKill :: Exception e => Weak ThreadId -> IORef Bool -> e -> IO ()
lockAndKill wtid ref e = do
    alreadyLocked <- atomicModifyIORef' ref (\b -> (True, b)) -- try to lock
    unless alreadyLocked $ do
        mtid <- deRefWeak wtid
        case mtid of
            Nothing -> return ()
            Just tid -> E.throwTo tid e

clear
    :: TVar (Map Key ManagedThread)
    -> (Key, Weak ThreadId, IORef Bool)
    -> IO ()
clear var (n, _, _) = atomically $ modifyTVar' var $ Map.delete n

ignore :: KilledByThreadManager -> IO ()
ignore (KilledByThreadManager _) = return ()

-- | Wait until all managed threads are finished.
waitUntilAllGone :: ThreadManager -> IO ()
waitUntilAllGone tm =
    atomically $
        isAllGone tm >>= check

-- | STM action that checks if all managed threads are finished.
isAllGone :: ThreadManager -> STM Bool
isAllGone tm = (== 0) <$> countManaged tm

-- | STM action for how many threads are managed, counting the places
--   kept by 'reserveManaged' for threads which have not started.
countManaged :: ThreadManager -> STM Int
countManaged (ThreadManager _timmgr var rvar) =
    (+) <$> (Map.size <$> readTVar var) <*> readTVar rvar

----------------------------------------------------------------

myWeakThreadId :: IO (Weak ThreadId, Key)
myWeakThreadId = do
    tid <- myThreadId
    wtid <- mkWeakThreadId tid
    let n = fromThreadId tid
    return (wtid, n)

labelMe :: String -> IO ()
labelMe l = do
    tid <- myThreadId
    labelThread tid l

-- | Registering a 'T.TimeoutAction' and unregister its 'T.Handle'
--   when the body action is finished.
withHandle
    :: ThreadManager -> T.TimeoutAction -> (T.Handle -> IO a) -> IO a
withHandle (ThreadManager timmgr _ _) = T.withHandle timmgr

#if __GLASGOW_HASKELL__ < 908
fromThreadId :: ThreadId -> Word64
fromThreadId tid = read (drop 9 $ show tid)
#endif
