{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module System.TimeManager.Internal where

import Control.Concurrent.MVar (MVar, modifyMVar, newMVar)
import Data.IORef (IORef, readIORef)
import Data.Word (Word64)

#if defined(mingw32_HOST_OS)
import qualified GHC.Event.Windows as EV
#else
import qualified GHC.Event as EV
#endif

----------------------------------------------------------------

-- | A timeout manager
newtype Manager = Manager Int

isNoManager :: Manager -> Bool
isNoManager (Manager 0) = True
isNoManager _ = False

----------------------------------------------------------------

-- | An action (callback) to be performed on timeout.
type TimeoutAction = IO ()

-- | A handle used by a timeout manager.
data Handle = Handle
    { handleTimeout :: Int
    , handleAction :: TimeoutAction
    , handleTimerManager :: ~TimerManager
    -- ^ The system timer manager the timeout key was registered with.
    --   Cached so that per-request operations don't re-fetch it.
    , handleState :: ~(IORef HandleState)
    -- ^ The current state. Used to decide whether a timeout is still going,
    -- paused, or completely terminated.
    --
    -- /We intentionally do not use an @MVar HandleState@ for performance reasons./
    , handleLastRenewed :: ~(IORef Word64)
    -- ^ Monotonic time (in nanoseconds) when the timeout was last
    --   registered or updated.
    , handleMinRenewGap :: Word64
    -- ^ 'tickle' is a no-op unless at least this many nanoseconds have
    --   passed since the last renewal.
    , handleLock :: Lock
    -- ^ Used by 'resume', 'pause' and 'cancel' to determine race conditions.
    --
    -- /We intentionally do not use an @MVar HandleState@ for performance reasons./
    -- /The lock only has to be grabbed to avoid race conditions./
    }

-- | Makes sure the function is only run when there's a key to act on.
withTimeoutKey :: Handle -> (EV.TimeoutKey -> IO ()) -> IO ()
withTimeoutKey h keyF = do
    st <- readIORef $ handleState h
    case st of
        Paused key -> keyF key
        Active key -> keyF key
        _ -> pure ()

-- | Makes sure the function is only run when the state is 'Active'.
withActiveTimeoutKey :: Handle -> (EV.TimeoutKey -> IO ()) -> IO ()
withActiveTimeoutKey h keyF = do
    st <- readIORef $ handleState h
    case st of
        Active key -> keyF key
        _ -> pure ()

-- | Used to avoid race conditions in situations when the state has to be changed.
type Lock = MVar ()

newLock :: IO Lock
newLock = newMVar ()

withLock :: Lock -> IO a -> IO a
withLock lock action =
    -- Not sure whether this should be 'modifyMVarMasked' or not.
    modifyMVar lock $ \l -> do
        a <- action
        pure (l, a)

-- | Tracking the state of a handle.
data HandleState
    = -- Timeout is primed to run
      Active EV.TimeoutKey
    | -- Timeout is paused, but still running
      -- ('resume' will set it back to 'Active' and 'tickle')
      Paused EV.TimeoutKey
    | -- Action ran, but timeout was paused, so it is resumable
      -- ('resume' will reregister the action)
      Stopped
    | -- Action was cancelled or run. 'register' is needed to start a new timeout.
      Terminated

isEmptyHandle :: Handle -> Bool
isEmptyHandle Handle{..} = handleTimeout == 0

withNonEmptyHandle :: Handle -> IO () -> IO ()
withNonEmptyHandle h act =
    if isEmptyHandle h then pure () else act

#if defined(mingw32_HOST_OS)
type TimerManager = EV.Manager

getTimerManager :: IO TimerManager
getTimerManager = EV.getSystemManager
#else
type TimerManager = EV.TimerManager

getTimerManager :: IO TimerManager
getTimerManager = EV.getSystemTimerManager
#endif
