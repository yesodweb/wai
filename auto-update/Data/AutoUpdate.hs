{-# LANGUAGE DeriveDataTypeable, RankNTypes #-}
-- FIXME: should we replace Data with System?
module Data.AutoUpdate where

import Control.Applicative ((<$>))
import Data.IORef
import Control.Concurrent (threadDelay, forkIOWithUnmask)
import Control.Monad (forever, void)
import Control.Exception (Exception, SomeException(..), handle, catches, throwIO, assert, Handler(..), mask)
import Data.Typeable (Typeable)

----------------------------------------------------------------

data UpdateSettings a = UpdateSettings
    { updateFreq :: !Int
    , updateSpawnThreshold :: !Int
    , updateAction :: !(IO a)
    }

data Status a = AutoUpdated !a !Int -- ^ # of manual update during updateFreq
              | SemiAuto
              | ManualUpdates !Int  -- ^ # of manual update

data AutoUpdate a = AutoUpdate
    { auSettings :: !(UpdateSettings a)
    , auStatus :: !(IORef (Status a))
    }

data Action a = Return a | Manual | Spawn

data Replaced = Replaced deriving (Show, Typeable)
instance Exception Replaced

----------------------------------------------------------------

mkAutoUpdate :: UpdateSettings a -> IO (AutoUpdate a)
mkAutoUpdate settings = AutoUpdate settings <$> newIORef (ManualUpdates 0)

getCurrent :: AutoUpdate a -> IO a
getCurrent au@(AutoUpdate (UpdateSettings _ spawnThreshold action) istatus) =
    handle handler update
  where
    update = mask $ \restore -> do
        ea <- atomicModifyIORef' istatus increment
        case ea of
            Return a -> return a
            Manual   -> restore action
            Spawn    -> do
                a <- restore action
                void $ forkIOWithUnmask $ spawn au a
                -- Without mask, an asynchronous exeption here may lead
                -- to spawn multiple threads.
                return a

    increment (AutoUpdated a cnt) = (AutoUpdated a (succ cnt), Return a)
    increment SemiAuto            = (SemiAuto, Manual)
    increment (ManualUpdates i)
      | i > spawnThreshold        = (SemiAuto, Spawn)
      | otherwise                 = (ManualUpdates (succ i), Manual)

    handler (SomeException ex) = do
        atomicModifyIORef' istatus reset
        throwIO ex

    -- Resetting to the safe status
    reset SemiAuto = (ManualUpdates 0, ())
    -- If status is AutoUpdated, an thread is running.
    -- So, let's keep the status.
    reset status   = (status, ())

----------------------------------------------------------------

spawn :: AutoUpdate a -> a -> (forall b. IO b -> IO b) -> IO ()
spawn (AutoUpdate (UpdateSettings freq _ action) istatus) x restore =
    update `catches` handlers
  where
    update = do
        void $ atomicModifyIORef' istatus (turnToAuto x)
        restore loop

    turnToAuto a _ = (AutoUpdated a 0, ())

    loop = forever $ do
        threadDelay freq
        a <- action
        doit <- atomicModifyIORef' istatus $ trunToManual a
        doit

    -- Normal case.
    trunToManual a (AutoUpdated _ cnt)
      | cnt >= 1                       = (AutoUpdated a 0, return ())
      | otherwise                      = (ManualUpdates 0, stop)
    -- This case must not happen.
    trunToManual _ _                   = assert False (ManualUpdates 0, stop)

    stop :: IO a
    stop = throwIO Replaced

    handlers :: [Handler ()]
    handlers = [Handler rhandler, Handler ohandler]

    rhandler Replaced = return ()
    ohandler (SomeException ex) = do
        -- This thread is killed.
        -- Resetting to the safe status.
        atomicModifyIORef' istatus $ \_ -> (ManualUpdates 0, ())
        throwIO ex
