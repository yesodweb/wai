{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module System.TimeManager.Internal where

import Data.IORef (IORef)
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
    , handleKeyRef :: ~(IORef EV.TimeoutKey)
    , handleState :: ~(IORef HandleState)
    , handleLastRenewed :: ~(IORef Word64)
    -- ^ Monotonic time (in nanoseconds) when the timeout was last
    --   registered or updated.
    , handleMinRenewGap :: Word64
    -- ^ 'tickle' is a no-op unless at least this many nanoseconds have
    --   passed since the last renewal.
    }

-- | Tracking the state of a handle, to be able to have 'resume'
-- act like a 'register' or 'tickle'.
data HandleState = Active | Stopped

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
