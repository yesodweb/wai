-- from https://jaspervdj.be/posts/2015-02-24-lru-cache.html
module Network.Wai.Middleware.Push.Referer.LRU (
    Cache(..)
  , Priority
  , empty
  , insert
  , lookup
  ) where

import Data.OrdPSQ (OrdPSQ)
import qualified Data.OrdPSQ as PSQ
import Data.Int (Int64)
import Prelude hiding (lookup)

import Network.Wai.Middleware.Push.Referer.Multi (Multi)
import qualified Network.Wai.Middleware.Push.Referer.Multi as M

type Priority = Int64

data Cache k v = Cache {
    cCapacity :: Int       -- ^ The maximum number of elements in the queue
  , cSize     :: Int       -- ^ The current number of elements in the queue
  , cValLimit :: Int
  , cTick     :: Priority  -- ^ The next logical time
  , cQueue    :: OrdPSQ k Priority (Multi v)
  } deriving (Eq, Show)

empty :: Int -> Int -> Cache k v
empty capacity valLimit
  | capacity < 1 = error "Cache.empty: capacity < 1"
  | otherwise    = Cache {
        cCapacity = capacity
      , cSize     = 0
      , cValLimit = valLimit
      , cTick     = 0
      , cQueue    = PSQ.empty
      }

trim :: Ord k => Cache k v -> Cache k v
trim c
  | cTick c == maxBound  = empty (cCapacity c) (cValLimit c)
  | cSize c > cCapacity c = c {
        cSize  = cSize c - 1
      , cQueue = PSQ.deleteMin (cQueue c)
      }
  | otherwise             = c

insert :: (Ord k, Ord v) => k -> v -> Cache k v -> Cache k v
insert k v c = case PSQ.alter lookupAndBump k (cQueue c) of
    (True,  q) -> trim $ c { cTick = cTick c + 1, cQueue = q, cSize = cSize c + 1}
    (False, q) -> trim $ c { cTick = cTick c + 1, cQueue = q }
  where
    lookupAndBump Nothing       = (True,  Just (cTick c, M.singleton (cValLimit c) v))
    lookupAndBump (Just (_, x)) = (False, Just (cTick c, M.insert v x))

lookup :: Ord k => k -> Cache k v -> (Cache k v, [v])
lookup k c = case PSQ.alter lookupAndBump k (cQueue c) of
    (Nothing, _) -> (c, [])
    (Just x, q)  -> let c' = trim $ c { cTick = cTick c + 1, cQueue = q }
                        xs = M.list x
                    in (c', xs)
  where
    lookupAndBump Nothing       = (Nothing, Nothing)
    lookupAndBump (Just (_, x)) = (Just x,  Just (cTick c, x))
