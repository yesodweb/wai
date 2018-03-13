module Network.Wai.Handler.Warp.MultiMap (
    MMap
  , isEmpty
  , empty
  , singleton
  , insert
  , search
  , searchWith
  , pruneWith
  , toList
  , merge
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import qualified Data.List.NonEmpty as NE
import Data.Semigroup
import Prelude -- Silence redundant import warnings

import Network.Wai.Handler.Warp.Imports hiding ((<>), union, empty, insert)

----------------------------------------------------------------

type MMap v = IntMap (NonEmpty v)

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
singleton k v = I.singleton k (v :| [])

----------------------------------------------------------------

-- | O(log n)
search :: Int -> MMap v -> Maybe v
search k m = case I.lookup k m of
    Nothing -> Nothing
    Just s  -> Just $! NE.head s

-- | O(log n)
searchWith :: Int -> (v -> Bool) -> MMap v -> Maybe v
searchWith k f m = case I.lookup k m of
    Nothing  -> Nothing
    Just nxs -> find f $ NE.toList nxs

----------------------------------------------------------------

-- | O(log n)
insert :: Int -> v -> MMap v -> MMap v
insert k v m = I.insertWith (<>) k (v :| []) m

----------------------------------------------------------------

-- | O(n)
toList :: MMap v -> [v]
toList m = concatMap f $ I.toAscList m
  where
    f (_,s) = NE.toList s

----------------------------------------------------------------

-- | O(n)
pruneWith :: MMap v
          -> (v -> IO Bool)
          -> IO (MMap v)
pruneWith m action = I.fromAscList <$> go (I.toDescList m) []
  where
    go []          acc = return acc
    go ((k,s):kss) acc = do
        mt <- prune action s
        case mt of
            Nothing -> go kss acc
            Just t  -> go kss ((k,t) : acc)

----------------------------------------------------------------

-- O(n + m) where N is the size of the second argument
merge :: MMap v -> MMap v -> MMap v
merge m1 m2 = I.unionWith (<>) m1 m2

----------------------------------------------------------------

prune :: (a -> IO Bool) -> NonEmpty a -> IO (Maybe (NonEmpty a))
prune act nxs = NE.nonEmpty <$> go (NE.toList nxs)
  where
    go []     = return []
    go (x:xs) = do
        keep <- act x
        rs <- go xs
        return $ if keep then x:rs else rs
