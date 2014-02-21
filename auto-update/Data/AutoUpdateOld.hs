{-# LANGUAGE DeriveDataTypeable #-}
module Data.AutoUpdate where

import Data.IORef
import Control.Concurrent (threadDelay, forkIO, ThreadId, killThread, myThreadId)
import Control.Monad (join, forever)
import Control.Exception (throwTo, Exception, handle, fromException, throwIO, assert)
import Data.Typeable (Typeable)

data UpdateSettings a = UpdateSettings
    { updateFreq :: !Int
    , updateSpawnThreshold :: !Int
    , updateAction :: !(IO a)
    }

data Status a = AutoUpdated !a !Int !ThreadId | ManualUpdates !Int
data AutoUpdate a = AutoUpdate
    { auSettings :: !(UpdateSettings a)
    , auStatus :: !(IORef (Status a))
    }

mkAutoUpdate :: UpdateSettings a -> IO (AutoUpdate a)
mkAutoUpdate settings = do
    istatus <- newIORef $ ManualUpdates 0
    return $ AutoUpdate settings istatus

data Action a = Return a | Manual | Spawn

getCurrent :: AutoUpdate a -> IO a
getCurrent (AutoUpdate (UpdateSettings freq spawnThreshold action) istatus) = do
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

data Replaced = Replaced
    deriving (Show, Typeable)
instance Exception Replaced
