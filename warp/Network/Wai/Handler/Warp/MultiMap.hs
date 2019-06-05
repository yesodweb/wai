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

import Data.Hashable (hash)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import Data.Semigroup
import Prelude -- Silence redundant import warnings

----------------------------------------------------------------

-- | 'MultiMap' is used for cache of file descriptors.
--   Since multiple threads would open file descriptors for
--   the same file simultaneously, multiple entries must
--   be contained for the file.
--   Since hash values of file pathes are used as outer keys,
--   collison would happen for multiple file pathes.
--   Becase only positive entries are contained,
--   a bad guy cannot be cause the hash collision intentinally.
--   So, lists are good enough.
newtype MultiMap v = MultiMap (IntMap [(FilePath,v)])

----------------------------------------------------------------

-- | O(1)
empty :: MultiMap v
empty = MultiMap $ I.empty

-- | O(1)
isEmpty :: MultiMap v -> Bool
isEmpty (MultiMap mm) = I.null mm

----------------------------------------------------------------

-- | O(1)
singleton :: FilePath -> v -> MultiMap v
singleton path v = MultiMap mm
  where
    !h = hash path
    !mm = I.singleton h [(path,v)]

----------------------------------------------------------------

-- | O(N)
lookup :: FilePath -> MultiMap v -> Maybe v
lookup path (MultiMap mm) = case I.lookup h mm of
    Nothing -> Nothing
    Just s  -> Prelude.lookup path s
  where
    !h = hash path

----------------------------------------------------------------

-- | O(log n)
insert :: FilePath -> v -> MultiMap v -> MultiMap v
insert path v (MultiMap mm) = MultiMap mm'
  where
    !h = hash path
    !mm' = I.insertWith (<>) h [(path,v)] mm

----------------------------------------------------------------

-- | O(n)
toList :: MultiMap v -> [(FilePath,v)]
toList (MultiMap mm) = concatMap snd $ I.toAscList mm

----------------------------------------------------------------

-- | O(n)
pruneWith :: MultiMap v
          -> ((FilePath,v) -> IO Bool)
          -> IO (MultiMap v)
pruneWith (MultiMap mm) action = MultiMap <$> mm'
  where
    !mm' = I.fromAscList <$> go (I.toDescList mm) []
    go []          !acc = return acc
    go ((h,s):kss) !acc = do
        rs <- prune action s
        case rs of
            [] -> go kss acc
            _  -> go kss ((h,rs) : acc)

----------------------------------------------------------------

-- O(n + m) where N is the size of the second argument
merge :: MultiMap v -> MultiMap v -> MultiMap v
merge (MultiMap m1) (MultiMap m2) = MultiMap mm
  where
    !mm = I.unionWith (<>) m1 m2

----------------------------------------------------------------

prune :: ((FilePath,v) -> IO Bool) -> [(FilePath,v)] -> IO [(FilePath,v)]
prune action xs0 = go xs0
  where
    go []     = return []
    go (x:xs) = do
        keep <- action x
        rs <- go xs
        return $ if keep then x:rs else rs
