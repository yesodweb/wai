-- | Lookup files stored in memory instead of from the filesystem.
module WaiAppStatic.Storage.Embedded.Runtime
    ( -- * Settings
      embeddedSettings
    ) where

import WaiAppStatic.Types
import Data.ByteString (ByteString)
import Control.Arrow ((&&&), second)
import Data.List
import Blaze.ByteString.Builder (fromByteString)
import qualified Network.Wai as W
import qualified Data.Map as Map
import Data.Function (on)
import qualified Data.Text as T
import Data.Ord
import qualified Data.ByteString as S
import Crypto.Hash (hash, MD5, Digest)
import Data.Byteable (toBytes)
import qualified Data.ByteString.Base64 as B64
import WaiAppStatic.Storage.Filesystem (defaultFileServerSettings)
import System.FilePath (isPathSeparator)

-- | Serve the list of path/content pairs directly from memory.
embeddedSettings :: [(Prelude.FilePath, ByteString)] -> StaticSettings
embeddedSettings files = (defaultFileServerSettings $ error "unused")
    { ssLookupFile = embeddedLookup $ toEmbedded files
    }

type Embedded = Map.Map Piece EmbeddedEntry

data EmbeddedEntry = EEFile ByteString | EEFolder Embedded

embeddedLookup :: Embedded -> Pieces -> IO LookupResult
embeddedLookup root pieces =
    return $ elookup pieces root
  where
    elookup  :: Pieces -> Embedded -> LookupResult
    elookup [] x = LRFolder $ Folder $ map toEntry $ Map.toList x
    elookup [p] x | T.null (fromPiece p) = elookup [] x
    elookup (p:ps) x =
        case Map.lookup p x of
            Nothing -> LRNotFound
            Just (EEFile f) ->
                case ps of
                    [] -> LRFile $ bsToFile p f
                    _ -> LRNotFound
            Just (EEFolder y) -> elookup ps y

toEntry :: (Piece, EmbeddedEntry) -> Either FolderName File
toEntry (name, EEFolder{}) = Left name
toEntry (name, EEFile bs) = Right File
    { fileGetSize = S.length bs
    , fileToResponse = \s h -> W.responseBuilder s h $ fromByteString bs
    , fileName = name
    , fileGetHash = return $ Just $ runHash bs
    , fileGetModified = Nothing
    }

toEmbedded :: [(Prelude.FilePath, ByteString)] -> Embedded
toEmbedded fps =
    go texts
  where
    texts = map (\(x, y) -> (filter (not . T.null . fromPiece) $ toPieces' x, y)) fps
    toPieces' "" = []
    toPieces' x =
        -- See https://github.com/yesodweb/yesod/issues/626
        --
        -- We want to separate on the forward slash on *all* OSes, and on
        -- Windows, also separate on a backslash.
        let (y, z) = break isPathSeparator x
         in unsafeToPiece (T.pack y) : toPieces' (drop 1 z)

    go :: [(Pieces, ByteString)] -> Embedded
    go orig =
        Map.fromList $ map (second go') hoisted
      where
        next = map (\(x, y) -> (head x, (tail x, y))) orig
        grouped :: [[(Piece, ([Piece], ByteString))]]
        grouped = groupBy ((==) `on` fst) $ sortBy (comparing fst) next
        hoisted :: [(Piece, [([Piece], ByteString)])]
        hoisted = map (fst . head &&& map snd) grouped

    go' :: [(Pieces, ByteString)] -> EmbeddedEntry
    go' [([], content)] = EEFile content
    go' x = EEFolder $ go $ filter (\y -> not $ null $ fst y) x

bsToFile :: Piece -> ByteString -> File
bsToFile name bs = File
    { fileGetSize = S.length bs
    , fileToResponse = \s h -> W.responseBuilder s h $ fromByteString bs
    , fileName = name
    , fileGetHash = return $ Just $ runHash bs
    , fileGetModified = Nothing
    }

runHash :: ByteString -> ByteString
runHash = B64.encode . toBytes . (hash :: S.ByteString -> Digest MD5)
