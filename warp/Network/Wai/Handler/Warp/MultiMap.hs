{-# LANGUAGE CPP, BangPatterns #-}

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

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Data.IntMap (IntMap)
import qualified Data.IntMap as I
import qualified Network.Wai.Handler.Warp.Some as S

----------------------------------------------------------------

type MMap v = IntMap (S.Some v)

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
singleton k v = I.singleton k (S.singleton v)

----------------------------------------------------------------

-- | O(log n)
search :: Int -> MMap v -> Maybe v
search k m = case I.lookup k m of
    Nothing -> Nothing
    Just s  -> Just $! S.top s

-- | O(log n)
searchWith :: Int -> (v -> Bool) -> MMap v -> Maybe v
searchWith k f m = case I.lookup k m of
    Nothing -> Nothing
    Just s  -> S.lookupWith f s

----------------------------------------------------------------

-- | O(log n)
insert :: Int -> v -> MMap v -> MMap v
insert k v m = I.insertWith S.union k (S.singleton v) m

----------------------------------------------------------------

-- | O(n)
toList :: MMap v -> [v]
toList m = concatMap f $ I.toAscList m
  where
    f (_,s) = S.toList s

----------------------------------------------------------------

-- | O(n)
pruneWith :: MMap v
          -> (v -> IO Bool)
          -> IO (MMap v)
pruneWith m action = I.fromAscList <$> go (I.toAscList m)
  where
    go [] = return []
    go ((k,s):kss) = do
        mt <- S.prune action s
        lst <- go kss
        case mt of
            Nothing -> return lst
            Just t  -> return $ (k,t) : lst

----------------------------------------------------------------

-- O(n + m) where N is the size of the second argument
merge :: MMap v -> MMap v -> MMap v
merge m1 m2 = I.unionWith S.union m1 m2
