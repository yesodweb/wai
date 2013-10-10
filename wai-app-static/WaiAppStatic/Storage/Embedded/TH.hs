{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings, MagicHash #-}
module WaiAppStatic.Storage.Embedded.TH(
    Etag
  , EmbeddableEntry(..)
  , mkSettings
) where

import Blaze.ByteString.Builder.ByteString (insertByteString)
import Codec.Compression.GZip (compress)
import Control.Applicative
import Data.ByteString.Unsafe (unsafePackAddressLen)
import Data.Either (lefts, rights)
import GHC.Exts (Int(..))
import Language.Haskell.TH
import Network.Mime (MimeType, defaultMimeLookup)
import System.IO.Unsafe (unsafeDupablePerformIO)
import WaiAppStatic.Types
import WaiAppStatic.Storage.Filesystem (defaultWebAppSettings)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
#if !MIN_VERSION_template_haskell(2, 8, 0)
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as BL8
#endif
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Network.Wai as W

-- | An Etag is used to return 304 Not Modified responses so the client does not need
--   to download resources a second time.  Usually the etag is built from a hash of
--   the content.  To disable Etags, you can pass the empty string.  This will cause the
--   content to be redownloaded on every request.
type Etag = T.Text

-- | Used at compile time to hold data about an entry to embed into the compiled executable.
data EmbeddableEntry = EmbeddableEntry {
    eLocation :: T.Text        -- ^ The location where this resource should be served from.  The
                               --   location can contain forward slashes (/) to simulate directories,
                               --   but must not end with a forward slash.
  , eMimeType :: MimeType      -- ^ The mime type.
  , eContent  :: Either (Etag, BL.ByteString) ExpQ
                    -- ^ The content itself.  The content can be given as a tag and bytestring,
                    --   in which case the content will be embedded directly into the execuatble.
                    --   Alternatively, the content can be given as a template haskell expression
                    --   returning @IO ('Etag', 'BL.ByteString')@ in which case this action will
                    --   be executed on every request to reload the content (this is useful
                    --   for a debugging mode).
}

-- | This structure is used at runtime to hold the entry.
data EmbeddedEntry = EmbeddedEntry {
    embLocation   :: !T.Text
  , embMime       :: !MimeType
  , embEtag       :: !B.ByteString
  , embCompressed :: !Bool
  , embContent    :: !B.ByteString
}

-- | This structure is used at runtime to hold the reload entries.
data ReloadEntry = ReloadEntry {
    reloadLocation :: !T.Text
  , reloadMime     :: !MimeType
  , reloadContent  :: IO (T.Text, BL.ByteString)
}

-- The use of unsafePackAddressLen is safe here because the length
-- is correct and we will only be reading from the bytestring, never
-- modifying it.
--
-- The only IO within unsafePackAddressLen is within newForeignPtr_ where
-- a new IORef is created as newIORef (NoFinalizers, []) to hold the finalizer
-- for the pointer.  Since the pointer for the content will never have a finalizer
-- added, we do not care if this finalizer IORef gets created more than once since
-- the IORef will always be holding (NoFinalizers, []).  Therefore
-- unsafeDupablePerformIO is safe.
bytestringE :: B.ByteString -> ExpQ
#if MIN_VERSION_template_haskell(2, 8, 0)
bytestringE b = [| unsafeDupablePerformIO (unsafePackAddressLen (I# $lenE) $ctE) |]
    where
        lenE = litE $ intPrimL $ toInteger $ B.length b
        ctE = litE $ stringPrimL $ B.unpack b
#else
bytestringE b =
    [| B8.pack $s |]
  where
    s = litE $ stringL $ B8.unpack b
#endif

bytestringLazyE :: BL.ByteString -> ExpQ
#if MIN_VERSION_template_haskell(2, 8, 0)
bytestringLazyE b = [| unsafeDupablePerformIO (unsafePackAddressLen (I# $lenE) $ctE) |]
    where
        lenE = litE $ intPrimL $ toInteger $ BL.length b
        ctE = litE $ stringPrimL $ BL.unpack b
#else
bytestringLazyE b =
    [| B8.pack $s |]
  where
    s = litE $ stringL $ BL8.unpack b
#endif

-- | A template haskell expression which creates either an EmbeddedEntry or ReloadEntry.
mkEntry :: EmbeddableEntry -> ExpQ
mkEntry (EmbeddableEntry loc mime (Left (etag, ct))) =
        [| Left $ EmbeddedEntry (T.pack $locE)
                                $(bytestringE mime)
                                $(bytestringE $ T.encodeUtf8 etag)
                                (1 == I# $compressedE)
                                $(bytestringLazyE ct')
        |]
    where
        locE = litE $ stringL $ T.unpack loc
        (compressed, ct') = tryCompress mime ct
        compressedE = litE $ intPrimL $ if compressed then 1 else 0

mkEntry (EmbeddableEntry loc mime (Right expr)) =
        [| Right $ ReloadEntry (T.pack $locE)
                               $(bytestringE mime)
                               $expr
        |]
    where
        locE = litE $ stringL $ T.unpack loc

-- | Converts an embedded entry to a file
embeddedToFile :: EmbeddedEntry -> File
embeddedToFile entry = File
    { fileGetSize = fromIntegral $ B.length $ embContent entry
    , fileToResponse = \s h ->
        let h' = if embCompressed entry
                    then h ++ [("Content-Encoding", "gzip")]
                    else h
         in W.responseBuilder s h' $ insertByteString $ embContent entry

    -- Usually the fileName should just be the filename not the entire path,
    -- but we need the whole path to make the lookup within lookupMime
    -- possible.  lookupMime is provided only with the File and from that
    -- we must find the mime type. Putting the path here is OK since
    -- within staticApp the fileName is used for directory listings which
    -- we have disabled.
    , fileName = unsafeToPiece $ embLocation entry
    , fileGetHash = return $ if B.null (embEtag entry)
                                 then Nothing
                                 else Just $ embEtag entry
    , fileGetModified = Nothing
    }

-- | Converts a reload entry to a file
reloadToFile :: ReloadEntry -> IO File
reloadToFile entry = do
    (etag, ct) <- reloadContent entry
    let etag' = T.encodeUtf8 etag
    return $ File
        { fileGetSize = fromIntegral $ BL.length ct
        , fileToResponse = \s h -> W.responseLBS s h ct
        -- Similar to above the entire path needs to be in the fileName.
        , fileName = unsafeToPiece $ reloadLocation entry
        , fileGetHash = return $ if T.null etag then Nothing else Just etag'
        , fileGetModified = Nothing
        }


-- | Build a static settings based on a filemap.
filemapToSettings :: M.HashMap T.Text (MimeType, IO File) -> StaticSettings
filemapToSettings mfiles = (defaultWebAppSettings "")
                              { ssLookupFile = lookupFile
                              , ssGetMimeType = lookupMime
                              }
    where
        piecesToFile p = T.intercalate "/" $ map fromPiece p

        lookupFile [] = return LRNotFound
        lookupFile p =
            case M.lookup (piecesToFile p) mfiles of
                Nothing -> return LRNotFound
                Just (_,act) -> LRFile <$> act

        lookupMime (File { fileName = p }) =
            case M.lookup (fromPiece p) mfiles of
                Just (mime,_) -> return mime
                Nothing -> return $ defaultMimeLookup $ fromPiece p

-- | Create a 'StaticSettings' from a list of entries.  Executed at run time.
entriesToSt :: [Either EmbeddedEntry ReloadEntry] -> StaticSettings
entriesToSt entries = hmap `seq` filemapToSettings hmap
    where
        embFiles = [ (embLocation e, (embMime e, return $ embeddedToFile e)) | e <- lefts entries]
        reloadFiles = [ (reloadLocation r, (reloadMime r, reloadToFile r)) | r <- rights entries]
        hmap = M.fromList $ embFiles ++ reloadFiles

-- | Create a 'StaticSettings' at compile time that embeds resources directly into the compiled
--   executable.  The embedded resources are precompressed (depending on mime type)
--   so that during runtime the resource can be served very quickly.
--
--   Because of GHC Template Haskell stage restrictions, you must define
--   the entries in a different module than where you create the 'StaticSettings'.
--   For example,
--
-- > {-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
-- > module A (mkEmbedded) where
-- > 
-- > import WaiAppStatic.Storage.Embedded
-- > import Crypto.Hash.MD5 (hashlazy)
-- > import qualified Data.ByteString.Lazy as BL
-- > import qualified Data.ByteString.Base64 as B64
-- > import qualified Data.Text as T
-- > import qualified Data.Text.Encoding as T
-- > 
-- > hash :: BL.ByteString -> T.Text
-- > hash = T.take 8 . T.decodeUtf8 . B64.encode . hashlazy
-- > 
-- > mkEmbedded :: IO [EmbeddableEntry]
-- > mkEmbedded = do
-- >     file <- BL.readFile "test.css"
-- >     let emb = EmbeddableEntry {
-- >                   eLocation = "somedir/test.css"
-- >                 , eMimeType = "text/css"
-- >                 , eContent  = Left (hash file, file)
-- >                 }
-- > 
-- >     let reload = EmbeddableEntry {
-- >                      eLocation = "anotherdir/test2.txt"
-- >                    , eMimeType = "text/plain"
-- >                    , eContent  = Right [| BL.readFile "test2.txt" >>= \c -> return (hash c, c) |]
-- >                    }
-- > 
-- >     return [emb, reload]
--
-- The above @mkEmbedded@ will be executed at compile time.  It loads the contents of test.css and
-- computes the hash of test.css for the etag.  The content will be available at the URL somedir/test.css.
-- Internally, 'embedApp' below will attempt to compress the content at compile time. The compression will
-- only happen if the compressed content is shorter than the original and the mime type is either text or
-- javascript.  If the content is compressed, at runtime the precomputed compressed content will be served
-- with the appropriate HTTP header. If 'embedApp' decides not to compress the content, it will be
-- served directly.
--
-- Secondly, @mkEmbedded@ creates a reloadable entry.  This will be available at the URL anotherdir/test2.txt.
-- Whenver a request comes in for anotherdir/test2.txt, the action inside the quasiquote in eContent will
-- be executed.  This will re-read the test2.txt file and recompute its hash.
--
-- Finally, here is a module which uses the above action to create a 'W.Application'.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- > module B where
-- > 
-- > import A
-- > import Network.Wai (Application)
-- > import Network.Wai.Application.Static (staticApp)
-- > import WaiAppStatic.Storage.Embedded
-- > import Network.Wai.Handler.Warp (run)
-- > 
-- > myApp :: Application
-- > myApp = staticApp $(mkSettings mkEmbedded)
-- > 
-- > main :: IO ()
-- > main = run 3000 myApp
mkSettings :: IO [EmbeddableEntry] -> ExpQ
mkSettings action = do
    entries <- runIO action
    [| entriesToSt $(listE $ map mkEntry entries) |]

shouldCompress :: MimeType -> Bool
shouldCompress m = "text/" `B.isPrefixOf` m || m `elem` extra
    where
        extra = [ "application/json"
                , "application/javascript"
                , "application/ecmascript"
                ]

-- | Only compress if the mime type is correct and the compressed text is actually shorter.
tryCompress :: MimeType -> BL.ByteString -> (Bool, BL.ByteString)
tryCompress mime ct
        | shouldCompress mime = (c, ct')
        | otherwise = (False, ct)
    where
        compressed = compress ct
        c = BL.length compressed < BL.length ct
        ct' = if c then compressed else ct
