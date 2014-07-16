{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
-- | A common problem is the desire to have an action run at a scheduled
-- interval, but only if it is needed. For example, instead of having
-- every web request result in a new @getCurrentTime@ call, we'd like to
-- have a single worker thread run every second, updating an @IORef@.
-- However, if the request frequency is less than once per second, this is
-- a pessimization, and worse, kills idle GC.
--
-- This library allows you to define actions which will either be
-- performed by a dedicated thread or, in times of low volume, will be
-- executed by the calling thread.
module Control.AutoUpdate
    ( UpdateSettings
    , defaultUpdateSettings
    , updateFreq
    , updateSpawnThreshold
    , updateAction
    , mkAutoUpdate
    ) where

import           Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import           Control.Exception  (Exception, assert, fromException, handle,
                                     throwIO, throwTo)
import           Control.Monad      (forever, join)
import           Data.IORef         (IORef, atomicModifyIORef, newIORef)
import           Data.Typeable      (Typeable)

-- | Default value for creating an @UpdateSettings@.
--
-- Since 0.1.0
defaultUpdateSettings :: UpdateSettings ()
defaultUpdateSettings = UpdateSettings
    { updateFreq = 1000000
    , updateSpawnThreshold = 3
    , updateAction = return ()
    }

-- | Settings to control how values are updated.
--
-- This should be constructed using @defaultUpdateSettings@ and record
-- update syntax, e.g.:
--
-- @
-- let set = defaultUpdateSettings { updateAction = getCurrentTime }
-- @
--
-- Since 0.1.0
data UpdateSettings a = UpdateSettings
    { updateFreq           :: Int
    -- ^ Microseconds between update calls. Same considerations as
    -- @threadDelay@ apply.
    --
    -- Default: 1 second (1000000)
    --
    -- Since 0.1.0
    , updateSpawnThreshold :: Int
    -- ^ How many times the data must be requested before we decide to
    -- spawn a dedicated thread.
    --
    -- Default: 3
    --
    -- Since 0.1.0
    , updateAction         :: IO a
    -- ^ Action to be performed to get the current value.
    --
    -- Default: does nothing.
    --
    -- Since 0.1.0
    }

data Status a = AutoUpdated
                    !a
                    {-# UNPACK #-} !Int
                    -- Number of times used since last updated.
                    {-# UNPACK #-} !ThreadId
                    -- Worker thread.
              | ManualUpdates
                    {-# UNPACK #-} !Int
                    -- Number of times used since we started/switched
                    -- off manual updates.

-- | A value which will be updated, either via a dedicated thread or when
-- requested. Request the current value with @getCurrent@.
--
-- Since 0.1.0
data AutoUpdate a = AutoUpdate
-- We use bang patterns on all arguments to ensure no bottoms leak in.
    { auFreq            :: {-# UNPACK #-} !Int
    , auSpawnThreadhold :: {-# UNPACK #-} !Int
    , auAction          :: !(IO ())
    , auStatus          :: !(IORef (Status a))
    }

-- | Generate an action which will either read from an automatically
-- updated value, or run the update action in the current thread.
--
-- Since 0.1.0
mkAutoUpdate :: UpdateSettings a -> IO (IO a)
mkAutoUpdate (UpdateSettings !f !t !a) = do
    istatus <- newIORef $ ManualUpdates 0
    return $! getCurrent f t a istatus

data Action a = Return a | Manual | Spawn

data Replaced = Replaced deriving (Show, Typeable)
instance Exception Replaced

-- | Get the current value, either fed from an auto-update thread, or
-- computed manually in the current thread.
--
-- Since 0.1.0
getCurrent :: Int -- ^ frequency
           -> Int -- ^ spawn threshold
           -> IO a -- ^ internal update action
           -> IORef (Status a) -- ^ mutable state
           -> IO a
getCurrent freq spawnThreshold action istatus = do
    ea <- atomicModifyIORef istatus $ \status ->
        case status of
            AutoUpdated a cnt tid -> (AutoUpdated a (succ cnt) tid, Return a)
            ManualUpdates i -> (ManualUpdates (succ i), if i > spawnThreshold then Spawn else Manual)
    case ea of
        Return a -> return a
        Manual -> action
        Spawn -> do
            a <- action
            tid <- forkIO spawn
            join $ atomicModifyIORef istatus $ \status ->
                case status of
                    AutoUpdated _ cnt old -> (AutoUpdated a cnt tid, throwTo old Replaced)
                    ManualUpdates cnt -> (AutoUpdated a cnt tid, return ())
            return a
  where
    spawn = handle onErr $ forever $ do
        threadDelay freq
        a <- action
        let stop = throwIO Replaced
        join $ atomicModifyIORef istatus $ \status ->
            case status of
                AutoUpdated _ cnt tid
                    | cnt >= 1 -> (AutoUpdated a 0 tid, return ())
                    | otherwise -> (ManualUpdates 0, stop)
                ManualUpdates i -> assert False (ManualUpdates i, stop)
    onErr ex =
        case fromException ex of
            Just Replaced -> return ()
            Nothing -> do
                tid <- myThreadId
                atomicModifyIORef istatus $ \status ->
                    case status of
                        AutoUpdated _ _ tid' | tid == tid' -> (ManualUpdates 0, ())
                        _ -> (status, ())
                throwIO ex
