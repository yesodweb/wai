{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}

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
module Control.AutoUpdate (
      -- * Type
      UpdateSettings
    , defaultUpdateSettings
      -- * Accessors
    , updateFreq
    , updateSpawnThreshold
    , updateAction
      -- * Creation
    , mkAutoUpdate
    ) where

import           Control.AutoUpdate.Util (atomicModifyIORef')
import           Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import           Control.Exception  (Exception, SomeException
                                    ,assert, fromException, handle,throwIO, throwTo)
import           Control.Monad      (forever, join)
import           Data.IORef         (IORef, newIORef)
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

-- | Generate an action which will either read from an automatically
-- updated value, or run the update action in the current thread.
--
-- Since 0.1.0
mkAutoUpdate :: UpdateSettings a -> IO (IO a)
mkAutoUpdate us = do
    istatus <- newIORef $ ManualUpdates 0
    return $! getCurrent us istatus

data Action a = Return a | Manual | Spawn

data Replaced = Replaced deriving (Show, Typeable)
instance Exception Replaced

-- | Get the current value, either fed from an auto-update thread, or
-- computed manually in the current thread.
--
-- Since 0.1.0
getCurrent :: UpdateSettings a
           -> IORef (Status a) -- ^ mutable state
           -> IO a
getCurrent us@UpdateSettings{..} istatus = do
    ea <- atomicModifyIORef' istatus increment
    case ea of
        Return a -> return a
        Manual   -> updateAction
        Spawn    -> do
            a <- updateAction
            tid <- forkIO $ spawn us istatus
            join $ atomicModifyIORef' istatus $ turnToAuto a tid
            return a
  where
    increment (AutoUpdated a cnt tid) = (AutoUpdated a (succ cnt) tid, Return a)
    increment (ManualUpdates i)       = (ManualUpdates (succ i),       act)
      where
        act = if i > updateSpawnThreshold then Spawn else Manual

    -- Normal case.
    turnToAuto a tid (ManualUpdates cnt)     = (AutoUpdated a cnt tid
                                               ,return ())
    -- Race condition: multiple threads were spawned.
    -- So, let's kill the previous one by this thread.
    turnToAuto a tid (AutoUpdated _ cnt old) = (AutoUpdated a cnt tid
                                               ,throwTo old Replaced)

spawn :: UpdateSettings a -> IORef (Status a) -> IO ()
spawn UpdateSettings{..} istatus = handle (onErr istatus) $ forever $ do
    threadDelay updateFreq
    a <- updateAction
    join $ atomicModifyIORef' istatus $ turnToManual a
  where
    -- Normal case.
    turnToManual a (AutoUpdated _ cnt tid)
      | cnt >= 1                     = (AutoUpdated a 0 tid, return ())
      | otherwise                    = (ManualUpdates 0, stop)
    -- This case must not happen.
    turnToManual _ (ManualUpdates i) =  assert False (ManualUpdates i, stop)

onErr :: IORef (Status a) -> SomeException -> IO ()
onErr istatus ex = case fromException ex of
    Just Replaced -> return ()
    Nothing -> do
        tid <- myThreadId
        atomicModifyIORef' istatus $ clear tid
        throwIO ex
  where
    -- In the race condition described above,
    -- suppose thread A is running, and is killed by thread B.
    -- Thread B then updates the IORef to refer to thread B.
    -- Then thread A's exception handler fires.
    -- We don't want to modify the IORef at all,
    -- since it refers to thread B already.
    -- Solution: only switch back to manual updates
    -- if the IORef is pointing at the current thread.
    clear tid (AutoUpdated _ _ tid') | tid == tid' = (ManualUpdates 0, ())
    clear _   status                               = (status, ())

stop :: IO a
stop = throwIO Replaced
