{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- | Timeout manager. Since v0.3.0, timeout manager is a wrapper of
-- GHC System TimerManager.
--
-- Users of old version should check the current semantics.
module System.TimeManager (
    -- ** Types
    Manager,
    defaultManager,
    TimeoutAction,
    Handle,
    emptyHandle,

    -- ** Manager
    initialize,
    stopManager,
    killManager,
    withManager,
    withManager',

    -- ** Registering a timeout action
    withHandle,
    withHandleKillThread,

    -- ** Control timeout
    tickle,
    pause,
    resume,

    -- ** Low level
    register,
    registerKillThread,
    cancel,

    -- ** Exceptions
    TimeoutThread (..),
) where

import Control.Concurrent (mkWeakThreadId, myThreadId)
import qualified Control.Exception as E
import Data.IORef (IORef)
import qualified Data.IORef as I
import System.Mem.Weak (deRefWeak)

#if defined(mingw32_HOST_OS)
import qualified GHC.Event.Windows as EV
#else
import qualified GHC.Event as EV
#endif

----------------------------------------------------------------

-- | A timeout manager
newtype Manager = Manager Int

-- | A manager whose timeout value is 0 (no callbacks are fired).
defaultManager :: Manager
defaultManager = Manager 0

isNoManager :: Manager -> Bool
isNoManager (Manager 0) = True
isNoManager _ = False

----------------------------------------------------------------

-- | An action (callback) to be performed on timeout.
type TimeoutAction = IO ()

----------------------------------------------------------------

-- | A handle used by a timeout manager.
data Handle = Handle
    { handleTimeout :: Int
    , handleAction :: TimeoutAction
    , handleKeyRef :: ~(IORef EV.TimeoutKey)
    , handleState :: ~(IORef HandleState)
    }

-- | Tracking the state of a handle, to be able to have 'resume'
-- act like a 'register' or 'tickle'.
data HandleState = Active | Stopped

-- | Dummy 'Handle'.
emptyHandle :: Handle
emptyHandle =
    Handle
        { handleTimeout = 0
        , handleAction = pure ()
        , handleKeyRef = error "time-manager: Handle.handleKeyRef not set"
        , handleState = error "time-manager: Handle.handleState not set"
        }

isEmptyHandle :: Handle -> Bool
isEmptyHandle Handle{..} = handleTimeout == 0

----------------------------------------------------------------

-- | Creating timeout manager with a timeout value in microseconds.
--
--   Setting the timeout to zero or lower (<= 0) will produce a
--   `defaultManager`.
initialize :: Int -> IO Manager
initialize = pure . Manager . max 0

----------------------------------------------------------------

-- | Obsoleted since version 0.3.0
--   Is now equivalent to @pure ()@.
stopManager :: Manager -> IO ()
stopManager _ = pure ()
{-# DEPRECATED stopManager "This function does nothing since version 0.3.0" #-}

-- | Obsoleted since version 0.3.0
--   Is now equivalent to @pure ()@.
killManager :: Manager -> IO ()
killManager _ = pure ()
{-# DEPRECATED killManager "This function does nothing since version 0.3.0" #-}

----------------------------------------------------------------

-- | Registering a timeout action and unregister its handle
--   when the body action is finished.
withHandle :: Manager -> TimeoutAction -> (Handle -> IO a) -> IO a
withHandle mgr onTimeout action
    | isNoManager mgr = action emptyHandle
    | otherwise = E.bracket (register mgr onTimeout) cancel action

-- | Registering a timeout action of killing this thread and
--   unregister its handle when the body action is killed or finished.
withHandleKillThread :: Manager -> TimeoutAction -> (Handle -> IO ()) -> IO ()
withHandleKillThread mgr onTimeout action
    | isNoManager mgr = action emptyHandle
    | otherwise =
        E.handle ignore $ E.bracket (registerKillThread mgr onTimeout) cancel action
  where
    ignore TimeoutThread = pure ()

----------------------------------------------------------------

-- | Registering a timeout action.
register :: Manager -> TimeoutAction -> IO Handle
register mgr@(Manager timeout) onTimeout
    | isNoManager mgr = pure emptyHandle
    | otherwise = do
        sysmgr <- getTimerManager
        key <- EV.registerTimeout sysmgr timeout onTimeout
        keyref <- I.newIORef key
        state <- I.newIORef Active
        let h =
                Handle
                    { handleTimeout = timeout
                    , handleAction = onTimeout
                    , handleKeyRef = keyref
                    , handleState = state
                    }
        pure h

-- | Unregistering the timeout.
cancel :: Handle -> IO ()
cancel h@Handle{..} = withNonEmptyHandle h $ do
    mgr <- getTimerManager
    key <- I.readIORef handleKeyRef
    EV.unregisterTimeout mgr key
    I.atomicWriteIORef handleState Stopped

-- | Extending the timeout.
tickle :: Handle -> IO ()
tickle h@Handle{..} = withNonEmptyHandle h $ do
    mgr <- getTimerManager
    key <- I.readIORef handleKeyRef
#if defined(mingw32_HOST_OS)
    EV.updateTimeout mgr key $ fromIntegral (handleTimeout `div` 1000000)
#else
    EV.updateTimeout mgr key handleTimeout
#endif

-- | This is identical to 'cancel'.
--   To resume timeout with the same 'Handle', 'resume' MUST be called.
--   Don't call 'tickle' for resumption.
pause :: Handle -> IO ()
pause = cancel

-- | Resuming the timeout.
resume :: Handle -> IO ()
resume h@Handle{..} = withNonEmptyHandle h $ do
    state <- I.readIORef handleState
    case state of
        Active -> tickle h
        Stopped -> do
            mgr <- getTimerManager
            key <- EV.registerTimeout mgr handleTimeout handleAction
            I.atomicWriteIORef handleKeyRef key
            I.atomicWriteIORef handleState Active

----------------------------------------------------------------

-- | The asynchronous exception thrown if a thread is registered via
-- 'registerKillThread'.
data TimeoutThread = TimeoutThread

instance E.Exception TimeoutThread where
    toException = E.asyncExceptionToException
    fromException = E.asyncExceptionFromException

instance Show TimeoutThread where
    show TimeoutThread = "Thread killed by timeout manager"

-- | Registering a timeout action of killing this thread.
--   'TimeoutThread' is thrown to the thread which called this
--   function on timeout. Catch 'TimeoutThread' if you don't
--   want to leak the asynchronous exception to GHC RTS.
registerKillThread :: Manager -> TimeoutAction -> IO Handle
registerKillThread m onTimeout = do
    wtid <- myThreadId >>= mkWeakThreadId
    -- First run the timeout action in case the child thread is masked.
    register m $
        onTimeout `E.finally` do
            mtid <- deRefWeak wtid
            case mtid of
                Nothing -> pure ()
                -- FIXME: forkIO to prevent blocking TimerManger
                Just tid' -> E.throwTo tid' TimeoutThread

----------------------------------------------------------------

-- | Call the inner function with a timeout manager.
withManager
    :: Int
    -- ^ timeout in microseconds
    -> (Manager -> IO a)
    -> IO a
withManager timeout f = initialize timeout >>= f

-- | Call the inner function with a timeout manager.
--   This is identical to 'withManager'.
withManager'
    :: Int
    -- ^ timeout in microseconds
    -> (Manager -> IO a)
    -> IO a
withManager' = withManager
{-# DEPRECATED withManager' "This function is the same as 'withManager' since version 0.3.0" #-}

#if defined(mingw32_HOST_OS)
getTimerManager :: IO EV.Manager
getTimerManager = EV.getSystemManager
#else
getTimerManager :: IO EV.TimerManager
getTimerManager = EV.getSystemTimerManager
#endif

withNonEmptyHandle :: Handle -> IO () -> IO ()
withNonEmptyHandle h act =
    if isEmptyHandle h then pure () else act
