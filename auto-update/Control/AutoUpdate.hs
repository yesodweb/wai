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

import           Control.Concurrent      (forkIO, threadDelay)
import           Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar,
                                          takeMVar, tryPutMVar, tryTakeMVar)
import           Control.Exception       (SomeException, catch, throw, mask_, try)
import           Control.Monad           (forever, void)
import           Data.IORef              (newIORef, readIORef, writeIORef)

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

    -- This is used to set a value in the currRef variable when the worker
    -- thread exits. In reality, that value should never be used, since the
    -- worker thread exiting only occurs if an async exception is thrown, which
    -- should only occur if there are no references to needsRunning left.
    -- However, this handler will make error messages much clearer if there's a
    -- bug in the implementation.
    let fillRefOnExit f = do
            eres <- try f
            case eres of
                Left e -> writeIORef currRef $ error $
                    "Control.AutoUpdate.mkAutoUpdate: worker thread exited with exception: "
                    ++ (show (e :: SomeException))
                Right () -> writeIORef currRef $ error $
                    "Control.AutoUpdate.mkAutoUpdate: worker thread exited normally, "
                    ++ "which should be impossible due to usage of forever"

    -- fork the worker thread immediately. Note that we mask async exceptions,
    -- but *not* in an uninterruptible manner. This will allow a
    -- BlockedIndefinitelyOnMVar exception to still be thrown, which will take
    -- down this thread when all references to the returned function are
    -- garbage collected, and therefore there is no thread that can fill the
    -- needsRunning MVar.
    --
    -- Note that since we throw away the ThreadId of this new thread and never
    -- calls myThreadId, normal async exceptions can never be thrown to it,
    -- only RTS exceptions.
    mask_ $ void $ forkIO $ fillRefOnExit $ forever $ do
        -- but block until a value is actually needed
        takeMVar needsRunning

        -- new value requested, so run the updateAction
        a <- catchSome $ updateAction us

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

-- | Turn a runtime exception into an impure exception, so that all @IO@
-- actions will complete successfully. This simply defers the exception until
-- the value is forced.
catchSome :: IO a -> IO a
catchSome act = Control.Exception.catch act $ \e -> return $ throw (e :: SomeException)
