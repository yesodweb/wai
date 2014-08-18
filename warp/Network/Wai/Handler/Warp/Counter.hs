module Network.Wai.Handler.Warp.Counter (
    Counter
  , newCounter
  , isZero
  , increase
  , decrease
  ) where

import Network.Wai.Handler.Warp.IORef
import Control.Applicative ((<$>))

newtype Counter = Counter (IORef Int)

newCounter :: IO Counter
newCounter = Counter <$> newIORef 0

isZero :: Counter -> IO Bool
isZero (Counter ref) = (== 0) <$> readIORef ref

increase :: Counter -> IO ()
increase (Counter ref) = atomicModifyIORef' ref $ \x -> (x + 1, ())

decrease :: Counter -> IO ()
decrease (Counter ref) = atomicModifyIORef' ref $ \x -> (x - 1, ())
