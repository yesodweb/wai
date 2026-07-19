{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

-- | Timeout manager. Since @v0.3.0@, timeout manager is a wrapper of
-- GHC System TimerManager.
--
-- Some caveats of using this package:
--
--   * Only works for GHC
--   * Only works with a threaded runtime
--   * Users of older versions should check the current semantics.
--   * Using 32-bit systems means the max timeout is @'maxBound' :: Int@
--     (2147483647) microseconds, which is less than 36 minutes.
--   * Using the same 'Handle' in different threads might cause issues in some
--     edge cases. (i.e. using cancel/pause in one thread, and resume in another)
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

import Control.Concurrent (forkIO, mkWeakThreadId, myThreadId)
import qualified Control.Exception as E
import Control.Monad (void, when)
import qualified Data.IORef as I
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import System.Mem.Weak (deRefWeak)
import System.TimeManager.Internal

#if defined(mingw32_HOST_OS)
import qualified GHC.Event.Windows as EV
#else
import qualified GHC.Event as EV
#endif

----------------------------------------------------------------

-- | A manager whose timeout value is 0 (no callbacks are fired).
defaultManager :: Manager
defaultManager = Manager 0

----------------------------------------------------------------

-- | Dummy 'Handle'.
emptyHandle :: Handle
emptyHandle =
    Handle
        { handleTimeout = 0
        , handleAction = pure ()
        , handleTimerManager = error "time-manager: Handle.handleTimerManager not set"
        , handleKeyRef = error "time-manager: Handle.handleKeyRef not set"
        , handleState = error "time-manager: Handle.handleState not set"
        , handleLastRenewed = error "time-manager: Handle.handleLastRenewed not set"
        , handleMinRenewGap = 0
        }

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
        -- The system timer manager is stable for the lifetime of the
        -- process (and even if it were replaced, e.g. around a fork,
        -- the key registered below would only be meaningful to the
        -- manager it was registered with). So fetch it once here and
        -- cache it in the 'Handle' instead of re-reading the global
        -- IORef on every tickle/pause/resume.
        sysmgr <- getTimerManager
        key <- EV.registerTimeout sysmgr timeout onTimeout
        keyref <- I.newIORef key
        state <- I.newIORef Active
        now <- getMonotonicTimeNSec
        lastRenewed <- I.newIORef now
        let h =
                Handle
                    { handleTimeout = timeout
                    , handleAction = onTimeout
                    , handleTimerManager = sysmgr
                    , handleKeyRef = keyref
                    , handleState = state
                    , handleLastRenewed = lastRenewed
                    , handleMinRenewGap = minRenewGap timeout
                    }
        pure h

-- | How long 'tickle' waits before actually renewing the timeout:
--   a quarter of the timeout, capped at one second. Skipping a renewal
--   inside this window only shortens the effective timeout by up to
--   this gap, but turns hot 'tickle' loops (one per chunk sent or
--   received) into a clock read and a comparison.
minRenewGap :: Int -> Word64
minRenewGap timeout = min oneSecond (microToNano timeout `div` 4)
  where
    oneSecond = 1000000000
    microToNano = (* 1000) . fromIntegral

-- | Unregistering the timeout.
cancel :: Handle -> IO ()
cancel h@Handle{..} = withNonEmptyHandle h $ do
    key <- I.readIORef handleKeyRef
    EV.unregisterTimeout handleTimerManager key
    I.atomicWriteIORef handleState Stopped

-- | Extending the timeout.
--
-- To keep frequent callers cheap, the renewal is rate-limited: it is
-- skipped unless at least a quarter of the timeout (capped at one
-- second) has passed since the timeout was last registered or updated.
--
-- Careful: this does NOT reactivate an already paused 'Handle'!
tickle :: Handle -> IO ()
tickle h@Handle{..} = withNonEmptyHandle h $ do
    now <- getMonotonicTimeNSec
    lastRenewed <- I.readIORef handleLastRenewed
    when (now - lastRenewed >= handleMinRenewGap) $ do
        key <- I.readIORef handleKeyRef
#if defined(mingw32_HOST_OS)
        EV.updateTimeout handleTimerManager key $
            fromIntegral (handleTimeout `div` 1000000)
#else
        EV.updateTimeout handleTimerManager key handleTimeout
#endif
        I.writeIORef handleLastRenewed now

-- | This is identical to 'cancel'.
--   To resume timeout with the same 'Handle', 'resume' MUST be called.
--   Don't call 'tickle' for resumption.
pause :: Handle -> IO ()
pause = cancel

-- | Resuming the timeout.
--
-- Works like 'tickle' if the 'Handle' wasn't 'pause'd or 'cancel'ed.
resume :: Handle -> IO ()
resume h@Handle{..} = withNonEmptyHandle h $ do
    state <- I.readIORef handleState
    case state of
        Active -> tickle h
        Stopped -> do
            key <- EV.registerTimeout handleTimerManager handleTimeout handleAction
            I.atomicWriteIORef handleKeyRef key
            I.atomicWriteIORef handleState Active
            now <- getMonotonicTimeNSec
            I.writeIORef handleLastRenewed now

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
                Just tid' -> void . forkIO $ E.throwTo tid' TimeoutThread

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
