{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.MultiMap (
    MultiMap
  , isEmpty
  , empty
  , singleton
  , insert
  , Network.Wai.Handler.Warp.MultiMap.lookup
  , pruneWith
  , toList
  , merge
  ) where

import Control.Monad (filterM)
import Data.Hashable (hash)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import Data.Semigroup
import Prelude -- Silence redundant import warnings

----------------------------------------------------------------

-- | 'MultiMap' is used as a cache of file descriptors.
--   Since multiple threads could open file descriptors for
--   the same file simultaneously, there could be multiple entries
--   for one file.
--   Since hash values of file paths are used as outer keys,
--   collison would happen for multiple file paths.
--   Because only positive entries are stored,
--   Malicious attack cannot cause the inner list to blow up.
--   So, lists are good enough.
newtype MultiMap v = MultiMap (IntMap [(FilePath,v)])

----------------------------------------------------------------

-- | O(1)
empty :: MultiMap v
empty = MultiMap I.empty

-- | O(1)
isEmpty :: MultiMap v -> Bool
isEmpty (MultiMap mm) = I.null mm

----------------------------------------------------------------

-- | O(1)
singleton :: FilePath -> v -> MultiMap v
singleton path v = MultiMap $ I.singleton (hash path) [(path,v)]

----------------------------------------------------------------

-- | O(M) where M is the number of entries per file
lookup :: FilePath -> MultiMap v -> Maybe v
lookup path (MultiMap mm) = case I.lookup (hash path) mm of
    Nothing -> Nothing
    Just s  -> Prelude.lookup path s

----------------------------------------------------------------

-- | O(log n)
insert :: FilePath -> v -> MultiMap v -> MultiMap v
insert path v (MultiMap mm) = MultiMap
  $ I.insertWith (<>) (hash path) [(path,v)] mm

----------------------------------------------------------------

-- | O(n)
toList :: MultiMap v -> [(FilePath,v)]
toList (MultiMap mm) = concatMap snd $ I.toAscList mm

----------------------------------------------------------------

-- | O(n)
pruneWith :: MultiMap v
          -> ((FilePath,v) -> IO Bool)
          -> IO (MultiMap v)
pruneWith (MultiMap mm) action
  = I.foldrWithKey go (pure . MultiMap) mm I.empty
  where
    go h s cont acc = do
      rs <- filterM action s
      case rs of
        [] -> cont acc
        _  -> cont $! I.insert h rs acc

----------------------------------------------------------------

-- O(n + m) where N is the size of the second argument
merge :: MultiMap v -> MultiMap v -> MultiMap v
merge (MultiMap m1) (MultiMap m2) = MultiMap $ I.unionWith (<>) m1 m2
