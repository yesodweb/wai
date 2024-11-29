{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A thread manager.
--   The manager has responsibility to kill worker threads.
module System.ThreadManager (
    ThreadManager,
    T.Manager,
    newThreadManager,
    stopAfter,
    forkManaged,
    forkManagedFinally,
    forkManagedUnmask,
    forkManagedTimeout,
    forkManagedTimeoutFinally,
    KilledByThreadManager (..),
    waitUntilAllGone,
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
import Data.IORef
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
data ThreadManager = ThreadManager T.Manager (TVar ManagedThreads)

type Key = Word64
type ManagedThreads = Map Key ManagedThread

----------------------------------------------------------------

-- 'IORef' prevents race between WAI TimeManager (TimeoutThread)
-- and stopAfter (KilledByThreadManager).
-- It is initialized with 'False' and turned into 'True' when locked.
-- The winner can throw an asynchronous exception.
data ManagedThread = ManagedThread (Weak ThreadId) (IORef Bool)

----------------------------------------------------------------

-- | Starting a thread manager.
--   Its action is initially set to 'return ()' and should be set
--   by 'setAction'. This allows that the action can include
--   the manager itself.
newThreadManager :: T.Manager -> IO ThreadManager
newThreadManager timmgr = ThreadManager timmgr <$> newTVarIO Map.empty

----------------------------------------------------------------

data KilledByThreadManager = KilledByThreadManager (Maybe SomeException)
    deriving (Show)

instance Exception KilledByThreadManager where
    toException = E.asyncExceptionToException
    fromException = E.asyncExceptionFromException

-- | Stopping the manager.
--
-- The action is run in the scope of an exception handler that catches all
-- exceptions (including asynchronous ones); this allows the cleanup handler
-- to cleanup in all circumstances. If an exception is caught, it is rethrown
-- after the cleanup is complete.
stopAfter :: ThreadManager -> IO a -> (Maybe SomeException -> IO ()) -> IO a
stopAfter (ThreadManager _timmgr var) action cleanup = do
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
        case ma of
            Left err -> cleanup (Just err) >> E.throwIO err
            Right a -> cleanup Nothing >> return a

----------------------------------------------------------------

-- | Fork managed thread
--
-- This guarantees that the thread ID is added to the manager's queue before
-- the thread starts, and is removed again when the thread terminates
-- (normally or abnormally).
forkManaged :: ThreadManager -> String -> IO () -> IO ()
forkManaged mgr label io =
    forkManagedUnmask mgr label $ \unmask -> unmask io

-- | Like 'forkManaged', but run action with exceptions masked
forkManagedUnmask
    :: ThreadManager -> String -> ((forall x. IO x -> IO x) -> IO ()) -> IO ()
forkManagedUnmask (ThreadManager _timmgr var) label io =
    void $ E.mask_ $ forkIOWithUnmask $ \unmask -> E.handle ignore $ do
        labelMe label
        E.bracket (setup var) (clear var) $ \_ -> io unmask

forkManagedTimeout :: ThreadManager -> String -> (T.Handle -> IO ()) -> IO ()
forkManagedTimeout (ThreadManager timmgr var) label io =
    void $ forkIO $ E.handle ignore $ do
        labelMe label
        E.bracket (setup var) (clear var) $ \(_n, wtid, ref) ->
            -- 'TimeoutThread' is ignored by 'withHandle'.
            T.withHandle timmgr (lockAndKill wtid ref T.TimeoutThread) io

forkManagedFinally :: ThreadManager -> String -> IO () -> IO () -> IO ()
forkManagedFinally mgr label io final = E.mask $ \restore ->
    forkManaged
        mgr
        label
        (E.try (restore io) >>= \(_ :: Either E.SomeException ()) -> final)

forkManagedTimeoutFinally
    :: ThreadManager -> String -> (T.Handle -> IO ()) -> IO () -> IO ()
forkManagedTimeoutFinally mgr label io final = E.mask $ \restore ->
    forkManagedTimeout
        mgr
        label
        (\th -> E.try (restore $ io th) >>= \(_ :: Either E.SomeException ()) -> final)

setup :: TVar (Map Key ManagedThread) -> IO (Key, Weak ThreadId, IORef Bool)
setup var = do
    (wtid, n) <- myWeakThradId
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

waitUntilAllGone :: ThreadManager -> IO ()
waitUntilAllGone (ThreadManager _timmgr var) = atomically $ do
    m <- readTVar var
    check (Map.size m == 0)

----------------------------------------------------------------

myWeakThradId :: IO (Weak ThreadId, Key)
myWeakThradId = do
    tid <- myThreadId
    wtid <- mkWeakThreadId tid
    let n = fromThreadId tid
    return (wtid, n)

labelMe :: String -> IO ()
labelMe l = do
    tid <- myThreadId
    labelThread tid l

withHandle :: ThreadManager -> T.TimeoutAction -> (T.Handle -> IO a) -> IO a
withHandle (ThreadManager timmgr _) = T.withHandle timmgr

#if __GLASGOW_HASKELL__ < 908
fromThreadId :: ThreadId -> Word64
fromThreadId tid = read (drop 9 $ show tid)
#endif
