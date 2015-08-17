{-# LANGUAGE CPP #-}

-- | A thread pool manager.
--   The manager has responsibility to spawn and kill
--   worker threads.
module Network.Wai.Handler.Warp.HTTP2.Manager (
    Manager
  , start
  , setAction
  , stop
  , spawnAction
  , replaceWithAction
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad (void)
import Data.Set (Set)
import qualified Data.Set as Set
import Network.Wai.Handler.Warp.IORef

----------------------------------------------------------------

data Command = Stop | Spawn | Replace ThreadId

data Manager = Manager (TQueue Command) (IORef (IO ()))

-- | Starting a thread pool manager.
--   Its action is initially set to 'return ()' and should be set
--   by 'setAction'. This allows that the action can include
--   the manager itself.
start :: IO Manager
start = do
    tset <- newThreadSet
    q <- newTQueueIO
    ref <- newIORef (return ())
    void $ forkIO $ go q tset ref
    return $ Manager q ref
  where
    go q tset ref = do
        x <- atomically $ readTQueue q
        case x of
            Stop           -> kill tset
            Spawn          -> next
            Replace oldtid -> do
                del tset oldtid
                next
      where
        next = do
            action <- readIORef ref
            newtid <- forkIO action
            add tset newtid
            go q tset ref

setAction :: Manager -> IO () -> IO ()
setAction (Manager _ ref) action = writeIORef ref action

stop :: Manager -> IO ()
stop (Manager q _) = atomically $ writeTQueue q Stop

spawnAction :: Manager -> IO ()
spawnAction (Manager q _) = atomically $ writeTQueue q Spawn

replaceWithAction :: Manager -> ThreadId -> IO ()
replaceWithAction (Manager q _) tid = atomically $ writeTQueue q $ Replace tid

----------------------------------------------------------------

newtype ThreadSet = ThreadSet (IORef (Set ThreadId))

newThreadSet :: IO ThreadSet
newThreadSet = ThreadSet <$> newIORef Set.empty

add :: ThreadSet -> ThreadId -> IO ()
add (ThreadSet ref) tid =
    atomicModifyIORef' ref (\set -> (Set.insert tid set, ()))

del :: ThreadSet -> ThreadId -> IO ()
del (ThreadSet ref) tid =
    atomicModifyIORef' ref (\set -> (Set.delete tid set, ()))

kill :: ThreadSet -> IO ()
kill (ThreadSet ref) = Set.toList <$> readIORef ref >>= mapM_ killThread
