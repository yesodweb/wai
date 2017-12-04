module Network.Wai.Handler.Warp.HashMap where

import Data.Hashable (hash)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Prelude hiding (lookup)

import Network.Wai.Handler.Warp.Imports hiding (insert, lookup)

type Hash = Int
newtype HashMap k v = HashMap (IntMap (Map k v))

hashByteString :: ByteString -> Hash
hashByteString = hash

empty :: HashMap k v
empty = HashMap I.empty

null :: HashMap k v -> Bool
null (HashMap hm) = I.null hm

insert :: Ord k => Hash -> k -> v -> HashMap k v -> HashMap k v
insert h k v (HashMap hm) = HashMap $ I.insertWith f h m hm
  where
    m = M.singleton k v
    f = M.union -- fimxe
{-# SPECIALIZE insert :: Hash -> String -> v -> HashMap String v -> HashMap String v #-}

lookup :: Ord k => Hash -> k -> HashMap k v -> Maybe v
lookup h k (HashMap hm) = I.lookup h hm >>= M.lookup k
{-# SPECIALIZE lookup :: Hash -> String -> HashMap String v -> Maybe v #-}
