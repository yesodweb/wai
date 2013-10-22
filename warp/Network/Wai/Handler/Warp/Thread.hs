{-# LANGUAGE DeriveDataTypeable #-}

module Network.Wai.Handler.Warp.Thread (
    forkIOwithBreakableForever
  , breakForever
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (handle, throw, mask_, Exception)
import Control.Monad (void, forever)
import Data.IORef
import Data.Typeable

data BreakForever = BreakForever deriving (Show, Typeable)

instance Exception BreakForever

forkIOwithBreakableForever :: a -> (IORef a -> IO ()) -> IO (IORef a)
forkIOwithBreakableForever ini action = do
    ref <- newIORef ini
    void . forkIO . handle stopPropagation . forever . mask_ $ action ref
    return ref

stopPropagation :: BreakForever -> IO ()
stopPropagation _ = return ()

breakForever :: IORef a -> IO a
breakForever ref = atomicModifyIORef ref $ \x -> (throw BreakForever, x)
