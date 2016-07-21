{-# LANGUAGE BangPatterns #-}

module Network.Wai.Middleware.Push.Referer.LimitMultiMap where

import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S

data LimitMultiMap k v = LimitMultiMap {
      limitKey :: !Int
    , limitVal :: !Int
    , multiMap :: !(Map k (Set v))
    } deriving (Eq, Show)

isEmpty :: LimitMultiMap k t -> Bool
isEmpty (LimitMultiMap _ _ m) = M.null m

empty :: Int -> Int -> LimitMultiMap k v
empty lk lv = LimitMultiMap lk lv M.empty

insert :: (Ord k, Ord v) => (k,v) -> LimitMultiMap k v -> LimitMultiMap k v
insert (k,v) (LimitMultiMap lk lv m)
  | siz <  lk = let !m' = M.alter  alt k m in LimitMultiMap lk lv m'
  | siz == lk = let !m' = M.adjust adj k m in LimitMultiMap lk lv m'
  | otherwise = error "insert"
  where
    siz = M.size m
    alt Nothing          = Just $ S.singleton v
    alt s@(Just set)
      | S.size set == lv = s
      | otherwise        = Just $ S.insert v set
    adj set
      | S.size set == lv = set
      | otherwise        = S.insert v set

lookup :: Ord k => k -> LimitMultiMap k v -> [v]
lookup k (LimitMultiMap _ _ m) = case M.lookup k m of
  Nothing  -> []
  Just set -> S.toList set
