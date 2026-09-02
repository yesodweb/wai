{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
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
--     edge cases. (i.e. using 'cancel'/'pause' in one thread, and 'resume' in another)
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
import Data.Bits (shiftR)
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
        , handleTimerManager = mutError "handleTimerManager"
        , handleState = mutError "handleState"
        , handleLastRenewed = mutError "handleLastRenewed"
        , handleMinRenewGap = 0
        , handleLock = mutError "handleLock"
        }
  where
    mutError s = error $ "time-manager: Handle." <> s <> " not set"

----------------------------------------------------------------

-- | Creating timeout manager with a timeout value in microseconds.
--
--   Setting the timeout to zero or lower @(<= 0)@ will produce a
--   `defaultManager`.
--
--   __WARNING for Windows users:__ /the precision of extending timeouts/
--   /is only full "seconds". The provided microseconds will be floored/
--   /to the first full second. (i.e. @initialize 2_500_000@ will get/
--   /get extended by 2 seconds on a 'tickle')/
--   /This also means timeouts of less than one second will not be extended/
--   /when using 'tickle'./
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

-- ============== NOTE ABOUT THREAD SAFETY ==============
--
-- The use of 'IORef's are fine in the current situation where
-- the 'TimeManager' is supposed to be used in a single thread.
--
-- The triggered action, though, is run by the Timer Manager
-- outside of the thread it was registered in.
-- This will potentially cause race conditions if we implement
-- anything that depends on the 'Handle's state.
--
-- Given the following:
--   - If run in one thread: 'register/tickle/pause/resume/cancel' never
--     overlap, making them devoid of race conditions in the general sense.
--   - We want to hit the Timer Manager as little as possible.
--   - We want to keep the 'resume/pause' surface functionality intact, while
--     not hitting the Timer Manager when we don't have to. This means not
--     cancelling the timeout on a pause, but rather mark the timeout paused.
--   - Not actually stopping the timeout on 'pause' introduces race
--     conditions, because the registered action will need to check the 'Handle'
--     state to see whether it should actually run (Active) or if it should
--     drop the action (Paused/Stopped).
--   - Not hitting the Timer Manager on a 'pause' will increase performance on
--     hot 'resume/pause' loops, like 'warp' has when using a streaming response.
--   - 'tickle' gets a sort of debounce to avoid repeated updates in hot loops.
--     - The debounce is 1/4 of the timeout, but we cap it to a maximum of 1 second.
--     - This means the registered action might run earlier than the timeout
--       would indicate; that difference going up to a maximum of 'handleMinRenewGap'.
--   - The following can happen:
--     - == The "Surprise Active" issue ==
--        A 'resume' might get called right after a 'Paused' registered action
--        starts running, and sets the state to 'Active' __before__ the action
--        inspects the 'Handle' state.
--     - == The "Dropped Active" issue ==
--        A 'resume' might get called right after a 'Paused' registered action
--        starts running, but inspects the state __before__ the action sets the
--        state to 'Stopped', and the registered action inspects the state
--        __before__ the 'resume' has set it to 'Active'. Essentially missing
--        the 'resume' completely.
--     - == The "Dropped Cancel" issue ==
--       A 'cancel' getting called right after a 'Paused' registered action
--       starts running, and cancelling __after__ the action reads the state
--       will have the registered action overwrite the state to 'Stopped',
--       when it shouldn't register a new action, but stop everything.
--     - A 'pause' should technically not be an issue, as it will only run when
--       the state is 'Active', but it is a function that changes the state, so
--       just to be cautious, we let it grab the lock.
--     - A 'tickle' in the same situation doesn't matter, as a 'tickle'
--       shouldn't activate a 'Paused' state. (and doesn't change any state)
--   - The "Surprise Active" issue can be mitigated by checking the
--     'handleLastRenewed' time and reregistering the timeout action with the
--     remaining amount of microseconds in the case where it has not yet been
--     'handleTimeout' amount of time.
--     - A 'tickle' could also cause this if the state was 'Active' all along,
--       but we'll accept the 'tickle' as being on time to extend the timeout.
--   - The "Dropped Active" issue is a bit more difficult to mitigate. We'll
--     need a lock to guarantee that either the activation of 'resume' is seen
--     by the registered action, or that the termination of the registered
--     action is seen by the 'resume'.
--   - The "Dropped Cancel" issue will also be avoided when using a lock.
--   - The lock will generally never be contested. It is there only for the
--     off-chance that a state-changing function runs JUST after the registered
--     action triggers. So in general, we don't expect the lock to reduce
--     performance noticeably.

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
        stateRef <- I.newIORef Stopped
        lock <- newLock
        lastRenewedRef <- I.newIORef =<< getMonotonicTimeNSec
        let h =
                Handle
                    { handleTimeout = timeout
                    , handleAction = onTimeout
                    , handleTimerManager = sysmgr
                    , handleState = stateRef
                    , handleLastRenewed = lastRenewedRef
                    , handleMinRenewGap = minRenewGap timeout
                    , handleLock = lock
                    }
        -- Just in case the timeout is only 1 microsecond and because of thread
        -- scheduling it runs before we can change the state to 'Active'
        withLock lock $ do
            key <- registerAdjustedTimeout h timeout
            now <- getMonotonicTimeNSec
            I.writeIORef lastRenewedRef now
            I.writeIORef stateRef $ Active key
        pure h

-- | This function needs a separate 'timeout' argument, because we might not
-- register the full amount of time when continuing a timeout that was started
-- a bit earlier. (cf. "Surprise Active" situation)
registerAdjustedTimeout :: Handle -> Int -> IO EV.TimeoutKey
registerAdjustedTimeout h@Handle{..} timeout = do
    originalKeyRef <-
        I.newIORef $
            error "System.TimeManager.registerAdjustedTimeout: originalKeyRef not filled"
    key <-
        EV.registerTimeout handleTimerManager timeout $
            adjustOnTimeout originalKeyRef h
    I.writeIORef originalKeyRef key
    pure key

-- | Wrapper around a registered action to ensure correct handling.
--
-- We basically need the 'Handle', but this is used before making the handle, so
adjustOnTimeout :: I.IORef EV.TimeoutKey -> Handle -> TimeoutAction
adjustOnTimeout originalKeyRef h@Handle{..} = do
    let writeState = I.atomicWriteIORef handleState
    -- Lock ensures we don't get race conditions.
    -- We return a boolean so that we don't run the (potentially long) action
    -- while holding on to the lock.
    shouldRun <- withLock handleLock $ do
        st <- I.readIORef handleState
        case st of
            -- We can check the @now - handleLastRenewed@ diff
            -- and 'threadDelay' the diff to make the timing better?
            -- @if diff > 'handleTimeout - 'handleMinRenewGap' then runTimeout@
            Active key -> do
                -- set state ref to 'Active'?
                ifSameKey key $ do
                    lastRenewed <- I.readIORef handleLastRenewed
                    now <- getMonotonicTimeNSec
                    let diff = fromIntegral $ now - lastRenewed
                    if diff > handleTimeout
                        -- Valid expiration of the timeout, we run the action
                        then do
                            -- We're going to run the action, so set the state
                            -- so that it won't be resumed.
                            writeState Cancelled
                            pure True
                        -- "Surprise Active" situation
                        else do
                            -- We reschedule, but with only the remaining time
                            let remainingTimeout = handleTimeout - diff
                            k <- registerAdjustedTimeout h remainingTimeout
                            writeState $ Active k
                            pure False
            -- We find this action being run after it's been paused. We write
            -- the state to 'Stopped' so that 'resume' knows to reregister the
            -- timeout action.
            Paused key ->
                ifSameKey key $ do
                    writeState Stopped
                    pure False
            -- 'Stopped' and 'Cancelled' mean the action shouldn't run.
            _ -> pure False
    when shouldRun handleAction
  where
    -- If the key in the state isn't the same as the one this action
    -- was registered with, then this action shouldn't run.
    -- (Technically, this situation shouldn't happen. but since the registered
    -- action only ever runs once, we can afford to be redundant)
    ifSameKey key f = do
        originalKey <- I.readIORef originalKeyRef
        if key == originalKey
            then f
            else pure False

-- | How long 'tickle' waits before actually renewing the timeout:
--   a quarter of the timeout, capped at one second. Skipping a renewal
--   inside this window only shortens the effective timeout by up to
--   this gap, but turns hot 'tickle' loops (one per chunk sent or
--   received) into a clock read and a comparison.
minRenewGap :: Int -> Word64
minRenewGap timeout =
    -- @shiftR 2 === divide by 4@
    min maxRenewDebounce (microToNano timeout `shiftR` 2)
  where
    microToNano = (* 1_000) . fromIntegral

-- | One second in nanoseconds
maxRenewDebounce :: Word64
maxRenewDebounce = 1_000_000_000

-- | Run 'f' if the minimum renew gap has been crossed.
whenRenew :: Handle -> IO () -> IO ()
whenRenew h f = do
    now <- getMonotonicTimeNSec
    lastRenewed <- I.readIORef $ handleLastRenewed h
    when (now - lastRenewed >= handleMinRenewGap h) f

-- | Unregistering the timeout.
--
-- The timeout can not be 'resume'd. To "resume" the timeout, you need to
-- 'register' again.
cancel :: Handle -> IO ()
cancel h@Handle{..} =
    withNonEmptyHandle h $
        -- "Dropped Cancel" remedy
        --
        -- We can eat a potential mutex pause here to avoid race conditions,
        -- because we don't expect 'cancel' to be called in hot loops.
        --
        -- (The race condition being: the 'Cancelled' state being overwritten
        -- because the 'cancel' runs JUST after the registered action starts
        -- running, sets the state to 'Cancelled', and then the registered
        -- action finishes and overwrites it to 'Stopped')
        withLock handleLock $ do
            withTimeoutKey h $ EV.unregisterTimeout handleTimerManager
            I.atomicWriteIORef handleState Cancelled

-- | Extending the timeout.
--
-- To keep frequent callers cheap, the renewal is rate-limited: it is
-- skipped unless at least a quarter of the timeout (capped at one
-- second) has passed since the timeout was last registered or updated.
--
-- Careful: this does NOT reactivate an already 'pause'd 'Handle'!
--
-- __WARNING for Windows users:__ /the precision of extending timeouts/
-- /is only full "seconds". The provided microseconds will be floored/
-- /to the first full second. (i.e. @initialize 2_500_000@ will get/
-- /extended by 2 seconds on a 'tickle')/
-- /This also means timeouts of less than one second will not be extended/
-- /when using 'tickle'./
tickle :: Handle -> IO ()
tickle h@Handle{..} =
    withNonEmptyHandle h $
        whenRenew h $
            withActiveTimeoutKey h $ \key -> do
                updateTheTimeout key
                now <- getMonotonicTimeNSec
                I.atomicWriteIORef handleLastRenewed now
  where
    -- For some reason the Windows implementation of 'updateTimeout' wants
    -- full seconds, instead of the microseconds that's used when registering...
    updateTheTimeout key =
        EV.updateTimeout handleTimerManager key
#if defined(mingw32_HOST_OS)
            (fromIntegral (handleTimeout `div` 1_000_000))
#else
            handleTimeout
#endif

-- | Pauses the timeout so you can 'resume' it later. Does not stop it entirely.
-- Use 'cancel' if you want to make sure the action will not be resumed.
--
-- To resume a timeout with the same 'Handle', 'resume' MUST be called.
-- Don't call 'tickle' for resumption.
pause :: Handle -> IO ()
pause h@Handle{..} =
    withNonEmptyHandle h $
        withLock handleLock . withActiveTimeoutKey h $
            I.atomicWriteIORef handleState . Paused

-- | Resuming the timeout.
--
-- Works like 'tickle' if the 'Handle' wasn't 'pause'd or 'cancel'ed.
resume :: Handle -> IO ()
resume h@Handle{..} =
    withNonEmptyHandle h $
        -- we ignore the key when paused, because we recheck the state after
        -- grabbing the lock.
        checkStateWith (\_ -> onPausedOrStopped) onPausedOrStopped
  where
    -- "Dropped Active" remedy
    --
    -- Grabbing the lock ensures 'resume' runs either before or after the
    -- registered action changes the state.
    onPausedOrStopped =
        withLock handleLock $ checkStateWith pausedF stoppedF
    checkStateWith onPaused onStopped = do
        state <- I.readIORef handleState
        case state of
            -- 'tickle' doesn't introduce race conditions, so can always be run.
            Active{} -> tickle h
            -- Abort when cancelled.
            Cancelled -> pure ()
            Paused k -> onPaused k
            Stopped -> onStopped
    pausedF k = do
        -- Set state to 'Active' before 'tickle'ing, because
        -- 'tickle' only runs when the state is 'Active'.
        activateTimeout k
        tickle h
    stoppedF = do
        key <- registerAdjustedTimeout h handleTimeout
        now <- getMonotonicTimeNSec
        I.atomicWriteIORef handleLastRenewed now
        activateTimeout key
    activateTimeout =
        I.atomicWriteIORef handleState . Active

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
