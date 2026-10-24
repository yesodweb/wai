{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A thread manager.
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
    forkManagedUnmaskFinally,
    forkManagedBy,
    forkManagedByFinally,
    forkManagedTimeout,
    forkManagedTimeoutFinally,

    -- * Synchronization
    waitUntilAllGone,
    isAllGone,
    countManaged,
) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (
    AssertionFailed (..),
    Exception (..),
    SomeException (..),
 )
import qualified Control.Exception as E
import Control.Monad (unless, void, when)
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

-- | Manager to own threads.
data ThreadManager = ThreadManager
    { managedThreads :: TVar ManagedThreads
    , reservedPlaces :: TVar Int
    -- ^ Places kept by 'reserveManaged' for threads not started yet.
    , managerStopped :: TVar Bool
    -- ^ Stopped by 'stopAfter'.  A thread which registers after that
    --   is killed on arrival: it was forked before the stop and would
    --   otherwise run on with nobody owning it.
    }

type Key = Word64
type ManagedThreads = Map Key ManagedThread

----------------------------------------------------------------

-- The 'IORef' prevents a race between the timeout of
-- 'forkManagedTimeout' and 'forkManagedTimeoutFinally', which the time
-- manager runs, and 'stopAfter'.  Both throw 'KilledByThreadManager'.
-- It is initialized with 'False' and turned into 'True' when locked.
-- The winner can throw an asynchronous exception.
data ManagedThread = ManagedThread (Weak ThreadId) (IORef Bool)

----------------------------------------------------------------

-- | Create a thread manager.
--
-- A manager owns threads; it does not time them out.  A caller which wants
-- both passes its own 'T.Manager' to 'forkManagedTimeout'.
newThreadManager :: IO ThreadManager
newThreadManager = do
    managedThreads <- newTVarIO Map.empty
    reservedPlaces <- newTVarIO 0
    managerStopped <- newTVarIO False
    return ThreadManager{..}

----------------------------------------------------------------

-- | A place kept for a thread that is not running yet.
--
--   Registering a thread needs its 'ThreadId', and it is the thread
--   itself which provides it, by asking for its own.  A caller which
--   forks through a function that hands no identity back cannot do it on
--   the thread's behalf, so until the thread arrives and registers
--   itself, what is counted is the place kept for it.
newtype Reservation = Reservation (IORef Bool)

-- | Keeping a place for a thread about to be forked.  It counts towards
--   'countManaged' from here, so that a manager stopped in between does
--   not see a connection that has been accepted as one that is gone.
--   A place that is never taken is never given back, so a caller which
--   reserves and then fails to fork leaves the manager counting it.
reserveManaged :: ThreadManager -> IO Reservation
reserveManaged ThreadManager{..} = do
    atomically $ modifyTVar' reservedPlaces (+ 1)
    Reservation <$> newIORef False

-- | Running the action of a thread which was reserved for, taking the
--   place kept for it: the thread records itself, so that 'stopAfter' can
--   reach it, and gives the reservation back.
--
--   The reservation is given back only once the thread is recorded, so
--   that it is counted throughout.
--   A thread which gets here after 'stopAfter' has stopped the manager is
--   killed rather than registered.
--
--   The reservation must be one kept by this manager.  Taking one kept by
--   another lowers this manager's count instead of that one's: the other
--   manager never reaches zero, and once this count goes negative,
--   'countManaged', 'isAllGone' and 'waitUntilAllGone' throw 'AssertionFailed'.
takeReservation :: ThreadManager -> Reservation -> IO () -> IO ()
takeReservation mgr rsv io = taking mgr rsv (return ()) $ \_ -> io

-- | Giving a place back, at most once however often this is called: the
--   thread which takes it gives it back, and so does a caller whose fork
--   throws, and only one of them can have it.
release :: ThreadManager -> Reservation -> IO ()
release ThreadManager{..} (Reservation ref) = do
    taken <- atomicModifyIORef' ref (\b -> (True, b))
    unless taken $ atomically $ modifyTVar' reservedPlaces (subtract 1)

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
--
-- This tells the managed threads to stop; it does not wait for them to be
-- gone, and neither does the cleanup handler.  They are the manager\'s
-- until they end, so 'countManaged' goes on counting them -- which is what
-- a caller waits on with 'waitUntilAllGone' before it frees what those
-- threads were using.
--
-- A thread whose place is still being kept by 'reserveManaged' is not told
-- but refused when it arrives, and never runs.
--
-- A thread which swallows the exception and never ends is therefore
-- counted for ever, and 'waitUntilAllGone' on such a manager never
-- returns.  Nothing here can make it end; a caller which cannot rule that
-- out should put its own timeout around the wait.
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
stopAfterWithResult ThreadManager{..} action cleanup = do
    E.mask $ \unmask -> do
        ma <- E.try $ unmask action
        -- The threads are left in the map rather than taken out of it:
        -- 'throwTo' waits for the exception to be taken, not for the
        -- thread to be gone, and a manager which reports that it owns
        -- nothing while its threads are still unwinding is no use to a
        -- caller which frees what they were using.  They should take
        -- themselves out as they end, and 'managerStopped' is what keeps
        -- new ones from arriving.
        m <- atomically $ do
            writeTVar managerStopped True
            readTVar managedThreads
        let ths = Map.elems m
            er = either Just (const Nothing) ma
            ex = KilledByThreadManager er
        forM_ ths $ \(ManagedThread wtid ref) -> lockAndKill wtid ref ex
        cleanup ma

----------------------------------------------------------------

-- | Fork a managed thread.
--
-- A place is kept for the thread before it is forked, so that it counts
-- towards 'countManaged' from this call rather than from the moment the
-- run-time system gives the thread its first step.  The thread adds
-- itself to the manager's queue as the first thing it does, takes the
-- place over, and is removed again when it terminates (normally or
-- abnormally).
forkManaged
    :: ThreadManager
    -> String
    -- ^ Thread name
    -> IO ()
    -- ^ Action
    -> IO ()
forkManaged mgr label io = forkManagedFinally mgr label io $ return ()

-- | Like 'forkManaged', but with a cleanup function, run regardless of how
--   the action ends.  An exception from the action is not caught: it ends
--   the thread as it would under 'forkManaged'.
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
    forkManagedUnmaskFinally mgr label (\restore -> restore io) final

-- | Like 'forkManaged', but run the action with exceptions masked
--   until the provided `unmask` is used. (i.e. @(forall x. IO x -> IO x)@)
forkManagedUnmask
    :: ThreadManager
    -> String
    -- ^ Thread name
    -> ((forall x. IO x -> IO x) -> IO ())
    -- ^ Action with unmask argument
    -> IO ()
forkManagedUnmask mgr label io = forkManagedUnmaskFinally mgr label io $ return ()

-- | Like 'forkManagedUnmask', but with a cleanup function, run regardless of
--   how the action ends.  An exception from the action is not caught: it
--   ends the thread as it would under 'forkManagedUnmask'.
forkManagedUnmaskFinally
    :: ThreadManager
    -> String
    -- ^ Thread name
    -> ((forall x. IO x -> IO x) -> IO ())
    -- ^ Action with unmask argument
    -> IO ()
    -- ^ Cleanup: will always run, regardless of how the thread ends
    -> IO ()
forkManagedUnmaskFinally mgr label io final =
    E.mask_ $
        forkManagedByFinally mgr (\body -> void $ forkIOWithUnmask body) label io final

-- | Fork a managed thread with a fork of the caller's own, for a caller
--   which does not fork with 'forkIO'.  (The @warp@ library, for example,
--   forks the thread serving a connection through a function of its
--   settings, so that the user can replace it.)
--
--   A place is kept for the thread before the fork, so that it counts
--   towards 'countManaged' from here rather than from the moment the
--   thread starts, and the place is given back whether the fork makes the
--   thread or throws.  A caller which forks by itself can do the same
--   with 'reserveManaged' and 'takeReservation', which this is made of.
forkManagedBy
    :: ThreadManager
    -> (((forall x. IO x -> IO x) -> IO ()) -> IO a)
    -- ^ The fork to make the thread with
    -> String
    -- ^ Thread name
    -> ((forall x. IO x -> IO x) -> IO ())
    -- ^ Action with unmask argument
    -> IO a
forkManagedBy mgr fork label io = forkManagedByFinally mgr fork label io $ return ()

-- | Like 'forkManagedBy', but with a cleanup function.
forkManagedByFinally
    :: ThreadManager
    -> (((forall x. IO x -> IO x) -> IO ()) -> IO a)
    -- ^ The fork to make the thread with
    -> String
    -- ^ Thread name
    -> ((forall x. IO x -> IO x) -> IO ())
    -- ^ Action with unmask argument
    -> IO ()
    -- ^ Cleanup: will always run, regardless of how the thread ends
    -> IO a
forkManagedByFinally mgr fork label io final =
    E.bracketOnError (reserveManaged mgr) (release mgr) $ \rsv ->
        fork $ \unmask -> managed mgr rsv label final $ \_ -> io unmask

-- | Fork a managed thread with a handle created by a timeout manager.
--
--   The time manager is the caller's, since this manager has none: owning a
--   thread and timing it out are separate things, and only this pair of
--   functions needs both.  What needs them together is the lock that keeps
--   the timeout and 'stopAfter' from both throwing at the thread.
forkManagedTimeout
    :: ThreadManager
    -> T.Manager
    -- ^ Time manager
    -> String
    -- ^ Thread name
    -> (T.Handle -> IO ())
    -- ^ Action with timeout handle
    -> IO ()
forkManagedTimeout mgr timmgr label io =
    forkManagedTimeoutFinally mgr timmgr label io $ return ()

-- | Like 'forkManagedTimeout', but with a cleanup function, run regardless of
--   how the action ends.  An exception from the action is not caught: it
--   ends the thread as it would under 'forkManagedTimeout'.
forkManagedTimeoutFinally
    :: ThreadManager
    -> T.Manager
    -- ^ Time manager
    -> String
    -- ^ Thread name
    -> (T.Handle -> IO ())
    -- ^ Action with timeout handle
    -> IO ()
    -- ^ Cleanup: will always run, regardless of how the thread ends
    -> IO ()
forkManagedTimeoutFinally mgr timmgr label io final =
    E.bracketOnError (reserveManaged mgr) (release mgr) $ \rsv ->
        void $ forkIO $
            managed mgr rsv label final $ \(_n, ManagedThread wtid ref) ->
                T.withHandle timmgr (lockAndKill wtid ref ex) io
  where
    ex = KilledByThreadManager Nothing

-- | The body of a managed thread, under its own label, taking the place
--   kept for it.
managed
    :: ThreadManager
    -> Reservation
    -> String
    -> IO ()
    -> ((Key, ManagedThread) -> IO ())
    -> IO ()
managed mgr rsv label final body = do
    labelMe label
    taking mgr rsv final body

-- | Running the body of a thread which had a place kept for it.  The
--   place is given back only once the thread is recorded, so that it is
--   counted throughout.
taking
    :: ThreadManager
    -> Reservation
    -> IO ()
    -> ((Key, ManagedThread) -> IO ())
    -> IO ()
taking mgr rsv final body =
    recorded mgr (give >> final) $ \key -> give >> body key
  where
    give = release mgr rsv

-- | Running the body of a managed thread with the thread recorded.
--
--   The cleanup runs regardless of how the body ends, and while the thread is still
--   recorded, so that a manager which has none left has none whose cleanup
--   is still running either.
--
--   A thread forked before the manager was stopped and scheduled after it
--   is not recorded and does not run its body: there is nobody left to
--   kill it, and running it is what the stop was for.
recorded
    :: ThreadManager
    -> IO ()
    -> ((Key, ManagedThread) -> IO ())
    -> IO ()
recorded mgr final body =
    E.handle ignore $
        E.bracket (setup mgr) (mapM_ $ clear mgr) $ \mkey ->
            mapM_ body mkey `E.finally` final

-- | Recording this thread, or 'Nothing' once the manager has stopped.
setup
    :: ThreadManager
    -> IO (Maybe (Key, ManagedThread))
setup ThreadManager{..} = do
    (wtid, n) <- myWeakThreadId
    ref <- newIORef False
    let ent = ManagedThread wtid ref
    -- asking to throw KilledByThreadManager to me
    atomically $ do
        stopped <- readTVar managerStopped
        if stopped
            then return Nothing
            else do
                modifyTVar' managedThreads $ Map.insert n ent
                return $ Just (n, ent)

lockAndKill :: Exception e => Weak ThreadId -> IORef Bool -> e -> IO ()
lockAndKill wtid ref e = do
    alreadyLocked <- atomicModifyIORef' ref (\b -> (True, b)) -- try to lock
    unless alreadyLocked $ do
        mtid <- deRefWeak wtid
        case mtid of
            Nothing -> return ()
            Just tid -> E.throwTo tid e

clear
    :: ThreadManager
    -> (Key, ManagedThread)
    -> IO ()
clear ThreadManager{..} (n, _) =
    atomically $ modifyTVar' managedThreads $ Map.delete n

ignore :: KilledByThreadManager -> IO ()
ignore (KilledByThreadManager _) = return ()

-- | Wait until all managed threads are finished.
--
--   After 'stopAfter' this waits for the threads it stopped to end, not
--   merely for them to have been told.  See there for the thread that
--   never ends.
--
--   A place kept by 'reserveManaged' and never taken is never given back,
--   so this never returns for a caller which reserves and then fails to
--   fork.
waitUntilAllGone :: ThreadManager -> IO ()
waitUntilAllGone tm =
    atomically $
        isAllGone tm >>= check

-- | STM action that checks if all managed threads are finished.
isAllGone :: ThreadManager -> STM Bool
isAllGone tm = (== 0) <$> countManaged tm

-- | STM action for how many threads are managed, counting the places
--   kept by 'reserveManaged' for threads which have not started, and the
--   threads 'stopAfter' has stopped which have not ended yet.
--
--   Throws 'AssertionFailed' if the count of places has gone negative,
--   which happens when a reservation is taken by a manager other than the
--   one that kept it.  A mistake of that kind which does not drive the
--   count negative goes unnoticed.
countManaged :: ThreadManager -> STM Int
countManaged ThreadManager{..} = do
    places <- readTVar reservedPlaces
    when (places < 0) $
        throwSTM $
            AssertionFailed $
                "System.ThreadManager: "
                    ++ show places
                    ++ " reserved places; a reservation was taken by a"
                    ++ " manager other than its own"
    (+ places) . Map.size <$> readTVar managedThreads

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

#if __GLASGOW_HASKELL__ < 908
fromThreadId :: ThreadId -> Word64
fromThreadId tid = read (drop 9 $ show tid)
#endif
