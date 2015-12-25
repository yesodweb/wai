module Network.Wai.Handler.Warp.HashMap where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

type Hash = Int
newtype HashMap k v = HashMap (IntMap (Map k v))

empty :: HashMap k v
empty = HashMap $ I.empty

null :: HashMap k v -> Bool
null (HashMap hm) = I.null hm

insert :: Ord k => Hash -> k -> v -> HashMap k v -> HashMap k v
insert h k v (HashMap hm) = HashMap $ I.insertWith f h m hm
  where
    m = M.singleton k v
    f = M.union -- fimxe

lookup :: Ord k => Hash -> k -> HashMap k v -> Maybe v
lookup h k (HashMap hm) = I.lookup h hm >>= M.lookup k
