{-# LANGUAGE CPP, BangPatterns #-}

-- | A thread pool manager.
--   The manager has responsibility to spawn and kill
--   worker threads.
module Network.Wai.Handler.Warp.HTTP2.Manager (
    Manager
  , start
  , setAction
  , stop
  , spawnAction
  , addMyId
  , deleteMyId
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Foldable
import Data.IORef
import Data.Set (Set)
import qualified Data.Set as Set

import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Settings
import qualified Network.Wai.Handler.Warp.Timeout as T

----------------------------------------------------------------

type Action = T.Manager -> IO ()

data Command = Stop | Spawn | Add ThreadId | Delete ThreadId

data Manager = Manager (TQueue Command) (IORef Action)

-- | Starting a thread pool manager.
--   Its action is initially set to 'return ()' and should be set
--   by 'setAction'. This allows that the action can include
--   the manager itself.
start :: Settings -> IO Manager
start set = do
    q <- newTQueueIO
    ref <- newIORef (\_ -> return ())
    timmgr <- T.initialize $ settingsTimeout set * 1000000
    void $ forkIO $ go q Set.empty ref timmgr
    return $ Manager q ref
  where
    go q !tset0 ref timmgr = do
        x <- atomically $ readTQueue q
        case x of
            Stop          -> kill tset0 >> T.killManager timmgr
            Spawn         -> next tset0
            Add    newtid -> let !tset = add newtid tset0
                             in go q tset ref timmgr
            Delete oldtid -> let !tset = del oldtid tset0
                             in go q tset ref timmgr
      where
        next tset = do
            action <- readIORef ref
            newtid <- forkIO (action timmgr)
            let !tset' = add newtid tset
            go q tset' ref timmgr

setAction :: Manager -> Action -> IO ()
setAction (Manager _ ref) action = writeIORef ref action

stop :: Manager -> IO ()
stop (Manager q _) = atomically $ writeTQueue q Stop

spawnAction :: Manager -> IO ()
spawnAction (Manager q _) = atomically $ writeTQueue q Spawn

addMyId :: Manager -> IO ()
addMyId (Manager q _) = do
    tid <- myThreadId
    atomically $ writeTQueue q $ Add tid

deleteMyId :: Manager -> IO ()
deleteMyId (Manager q _) = do
    tid <- myThreadId
    atomically $ writeTQueue q $ Delete tid

----------------------------------------------------------------

add :: ThreadId -> Set ThreadId -> Set ThreadId
add tid set = set'
  where
    !set' = Set.insert tid set

del :: ThreadId -> Set ThreadId -> Set ThreadId
del tid set = set'
  where
    !set' = Set.delete tid set

kill :: Set ThreadId -> IO ()
kill set = traverse_ killThread set
