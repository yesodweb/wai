module Network.Wai.Handler.Warp.Thread (
    forkIOwithBreakableForever
  , breakForever
  ) where

import Control.Concurrent (forkIO)
import Control.Monad (void, forever)
import Control.Exception (handle, throw, mask_, SomeException, AsyncException(ThreadKilled))
import Data.IORef

forkIOwithBreakableForever :: a -> (IORef a -> IO ()) -> IO (IORef a)
forkIOwithBreakableForever ini action = do
    ref <- newIORef ini
    void . forkIO . handle stopPropagation . forever . mask_ $ action ref
    return ref

stopPropagation :: SomeException -> IO ()
stopPropagation _ = return ()

breakForever :: IORef a -> IO a
breakForever ref = atomicModifyIORef ref $ \x -> (throw ThreadKilled, x)
