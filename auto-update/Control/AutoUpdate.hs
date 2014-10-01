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

import           Control.Applicative ((<$>), (<*>))
import           Control.AutoUpdate.Util (atomicModifyIORef')
import           Control.Concurrent (ThreadId, forkIO, myThreadId, threadDelay)
import           Control.Exception  (Exception, SomeException
                                    ,assert, fromException, handle,throwIO, throwTo, throw, try, catch)
import           Control.Monad      (forever, join, void)
import           Data.IORef         (IORef, newIORef, readIORef, writeIORef)
import           Data.Typeable      (Typeable)
import           System.IO.Unsafe   (unsafePerformIO)
import Prelude hiding (catch)

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
    used <- newIORef False
    ref <- newIORef $ error "mkAutoUpdate"
    writeIORef ref $ unsafePerformIO $ start us used ref
    return $ do
        writeIORef used True
        readIORef ref

start settings@UpdateSettings{..} used ref = do
    a <- updateAction
    void $ forkIO $
        let loop = do
                threadDelay updateFreq
                used' <- readIORef used
                if used'
                    then do
                        writeIORef used False
                        a <- catchSome updateAction
                        writeIORef ref a
                        loop
                    else writeIORef ref $ unsafePerformIO $ start settings used ref
         in loop
    return a

catchSome :: IO a -> IO a
catchSome act = catch act $ \e -> return $ throw (e :: SomeException)
