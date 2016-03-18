{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.MultiMap (
    MMap
  , Some(..)
  , empty
  , singleton
  , insert
  , search
  , searchWith
  , isEmpty
  , pruneWith
  , toList
  , fromSortedList
  , toSortedList
  , merge
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Data.IntMap (IntMap)
import qualified Data.IntMap as I

----------------------------------------------------------------

-- | One ore more list to implement multimap.
data Some a = One !a
            | Tom !a !(Some a) -- Two or more
            deriving (Eq,Show)

{-
cons :: Some a -> a -> Some a
cons s x = Tom x s
-}

top :: Some a -> a
top (One x)   = x
top (Tom x _) = x

union :: Some a -> Some a -> Some a
union (One x) s = Tom x s
union (Tom x xs) s = union xs (Tom x s)

----------------------------------------------------------------

type MMap v = IntMap (Some v)

----------------------------------------------------------------

-- | O(log n)
search :: Int -> MMap v -> Maybe v
search k m = case I.lookup k m of
    Nothing -> Nothing
    Just s  -> Just $! top s

-- | O(log n)
searchWith :: Int -> (Some v -> Maybe v) -> MMap v -> Maybe v
searchWith k f m = case I.lookup k m of
    Nothing -> Nothing
    Just s  -> f s

----------------------------------------------------------------

-- | O(1)
isEmpty :: MMap v -> Bool
isEmpty = I.null

-- | O(1)
empty :: MMap v
empty = I.empty

----------------------------------------------------------------

-- | O(1)
singleton :: Int -> v -> MMap v
singleton k v = I.singleton k (One v)

----------------------------------------------------------------

-- | O(log n)
insert :: Int -> v -> MMap v -> MMap v
insert k v m = I.insertWith union k (One v) m

----------------------------------------------------------------

-- | O(n)
toList :: MMap v -> [v]
toList m = concatMap f $ I.toAscList m
  where
    f (_,s) = go s []
      where
        go (One x)    acc = x : acc
        go (Tom x xs) acc = go xs (x : acc)

----------------------------------------------------------------

-- | O(n)
fromSortedList :: [(Int,Some v)] -> MMap v
fromSortedList = I.fromAscList

----------------------------------------------------------------

-- | O(n)
toSortedList :: MMap v -> [(Int,Some v)]
toSortedList = I.toAscList

----------------------------------------------------------------

-- | O(n)
pruneWith :: MMap v
          -> ((Int,Some v) -> IO [(Int, Some v)])
          -> IO (MMap v)
pruneWith m action = fromSortedList . concat <$> mapM action (toSortedList m)

----------------------------------------------------------------

-- O(n + m) where N is the size of the second argument
merge :: MMap v -> MMap v -> MMap v
merge m1 m2 = I.unionWith union m1 m2
