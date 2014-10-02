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
    , updateCalcsPerRequest
      -- * Creation
    , mkAutoUpdate
    ) where

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar,
                                          takeMVar, tryPutMVar, tryTakeMVar)
import           Control.Exception       (SomeException, catch, throw)
import           Control.Monad           (forever, replicateM_, void)
import           Data.IORef              (newIORef, readIORef, writeIORef)

-- | Default value for creating an @UpdateSettings@.
--
-- Since 0.1.0
defaultUpdateSettings :: UpdateSettings ()
defaultUpdateSettings = UpdateSettings
    { updateFreq = 1000000
    , updateSpawnThreshold = 3
    , updateAction = return ()
    , updateCalcsPerRequest = 30
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
    -- ^ NOTE: This value no longer has any effect, since worker threads are
    -- dedicated instead of spawned on demand.
    --
    -- Previously, this determined: How many times the data must be requested
    -- before we decide to spawn a dedicated thread.
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
    , updateCalcsPerRequest :: Int
    -- ^ How many calculations to perform each time a value is requested.
    --
    -- Increasing this value means less time is spent checking if a requester
    -- actually needs a value, but also means that the worker thread may end up
    -- running longer than is necessary. This is essentially a
    -- performance/power consumption tradeoff, and depends on your
    -- application's needs.
    --
    -- Default: 30
    --
    -- Since 0.1.2
    }

-- | Generate an action which will either read from an automatically
-- updated value, or run the update action in the current thread.
--
-- Since 0.1.0
mkAutoUpdate :: UpdateSettings a -> IO (IO a)
mkAutoUpdate us = do
    -- The current value, if available.
    currRef <- newIORef Nothing

    -- A baton to tell the worker thread to generate a new value.
    needsRunning <- newEmptyMVar

    -- The last value generated, to allow for blocking semantics when currRef
    -- is Nothing.
    lastValue <- newEmptyMVar

    -- fork the worker thread immediately...
    void $ forkIO $ forever $ do
        -- but block until a value is actually needed
        takeMVar needsRunning

        -- new value requested, so run the updateAction
        a <- catchSome $ updateAction us

        -- calculate multiple times per filling up needsRunning to avoid MVar
        -- contention
        replicateM_ calcsPerRequest $ do
            -- we got a new value, update currRef and lastValue
            writeIORef currRef $ Just a
            void $ tryTakeMVar lastValue
            putMVar lastValue a

            -- delay until we're needed again
            threadDelay $ updateFreq us

        -- delay's over, clear out currRef and lastValue so that demanding the
        -- value again forces us to start work
        writeIORef currRef Nothing
        void $ takeMVar lastValue

    return $ do
        mval <- readIORef currRef
        case mval of
            -- we have a current value, use it
            Just val -> return val
            Nothing -> do
                -- no current value, force the worker thread to run...
                void $ tryPutMVar needsRunning ()

                -- and block for the result from the worker
                readMVar lastValue
  where
    calcsPerRequest = max 1 (updateCalcsPerRequest us)

-- | Turn a runtime exception into an impure exception, so that all @IO@
-- actions will complete successfully. This simply defers the exception until
-- the value is forced.
catchSome :: IO a -> IO a
catchSome act = Control.Exception.catch act $ \e -> return $ throw (e :: SomeException)
