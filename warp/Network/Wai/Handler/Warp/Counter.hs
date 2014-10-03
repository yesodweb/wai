module Network.Wai.Handler.Warp.Counter (
    Counter
  , newCounter
  , isZero
  , increase
  , decrease
  ) where

import qualified Data.Atomics.Counter.Unboxed as C
import Control.Applicative ((<$>))

newtype Counter = Counter C.AtomicCounter

newCounter :: IO Counter
newCounter = Counter <$> C.newCounter 0

isZero :: Counter -> IO Bool
isZero (Counter ref) = (== 0) <$> C.readCounter ref

increase :: Counter -> IO ()
increase (Counter ref) = C.incrCounter_ 1 ref

decrease :: Counter -> IO ()
decrease (Counter ref) = C.incrCounter_ (-1) ref
