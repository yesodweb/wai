-- from https://jaspervdj.be/posts/2015-02-24-lru-cache.html
module Network.Wai.Middleware.Push.Referer.LRU where

import qualified Data.OrdPSQ as OrdPSQ
import Data.Int (Int64)
import Data.Maybe (isNothing)
import Prelude hiding (lookup)

type Priority = Int64

data Cache k v = Cache {
    cCapacity :: Int       -- ^ The maximum number of elements in the queue
  , cSize     :: Int       -- ^ The current number of elements in the queue
  , cTick     :: Priority  -- ^ The next logical time
  , cQueue    :: OrdPSQ.OrdPSQ k Priority v
  } deriving (Eq, Show)

empty :: Int -> Cache k v
empty capacity
  | capacity < 1 = error "Cache.empty: capacity < 1"
  | otherwise    = Cache {
        cCapacity = capacity
      , cSize     = 0
      , cTick     = 0
      , cQueue    = OrdPSQ.empty
      }

trim :: Ord k => Cache k v -> Cache k v
trim c
  | cTick c == maxBound  = empty (cCapacity c)
  | cSize c > cCapacity c = c {
        cSize  = cSize c - 1
      , cQueue = OrdPSQ.deleteMin (cQueue c)
      }
  | otherwise             = c

insert :: Ord k => k -> v -> Cache k v -> Cache k v
insert key val c = trim c'
  where
    (mbOldVal, queue) = OrdPSQ.insertView key (cTick c) val (cQueue c)
    c'= c {
        cSize  = if isNothing mbOldVal then cSize c + 1 else cSize c
      , cTick  = cTick c + 1
      , cQueue = queue
      }

lookup :: Ord k => k -> Cache k v -> Maybe (v, Cache k v)
lookup k c = case OrdPSQ.alter lookupAndBump k (cQueue c) of
    (Nothing, _) -> Nothing
    (Just x, q)  -> let c' = trim $ c { cTick = cTick c + 1, cQueue = q }
                    in Just (x, c')
  where
    lookupAndBump Nothing       = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x,  Just ((cTick c), x))
