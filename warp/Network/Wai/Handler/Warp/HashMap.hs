{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.HashMap where

import Data.Hashable (hash)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

----------------------------------------------------------------

-- | 'HashMap' is used for cache of file information.
--   Hash values of file pathes are used as outer keys.
--   Because negative entries are also contained,
--   a bad guy can intentionally cause the hash collison.
--   So, 'Map' is used internally to prevent
--   the hash collision attack.
newtype HashMap v = HashMap (IntMap (Map FilePath v))

----------------------------------------------------------------

empty :: HashMap v
empty = HashMap I.empty

isEmpty :: HashMap v -> Bool
isEmpty (HashMap hm) = I.null hm

----------------------------------------------------------------

insert :: FilePath -> v -> HashMap v -> HashMap v
insert path v (HashMap hm) = HashMap
  $ I.insertWith M.union (hash path) (M.singleton path v) hm

lookup :: FilePath -> HashMap v -> Maybe v
lookup path (HashMap hm) = I.lookup (hash path) hm >>= M.lookup path