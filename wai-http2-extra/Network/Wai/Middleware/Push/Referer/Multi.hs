{-# LANGUAGE RecordWildCards #-}

module Network.Wai.Middleware.Push.Referer.Multi where

import Data.Set (Set)
import qualified Data.Set as Set

data Multi a = Multi
    { limit :: Int
    , list :: [a]
    , check :: Set a
    }
    deriving (Eq, Show)

empty :: Int -> Multi a
empty n = Multi n [] Set.empty

singleton :: Int -> a -> Multi a
singleton n v = Multi n [v] $ Set.singleton v

insert :: Ord a => a -> Multi a -> Multi a
insert _ m@Multi{..}
    | Set.size check == limit = m
insert v m@Multi{..}
    | Set.size check == Set.size check' = m
    | otherwise = Multi limit (v : list) check'
  where
    check' = Set.insert v check
