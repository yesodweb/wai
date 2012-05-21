{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
module Backend.Filesystem where

import Types
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import Data.List (foldl')
import Control.Monad (forM)
import Util
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Control.Exception (SomeException, try)
import qualified Network.Wai as W
import Listing
import Mime
import System.PosixCompat.Files (fileSize, getFileStatus, modificationTime)

pathFromPieces :: FilePath -> Pieces -> FilePath
pathFromPieces = foldl' (\fp p -> fp </> F.fromText (fromPiece p))

defaultWebAppSettings :: StaticSettings
defaultWebAppSettings = StaticSettings
    { ssLookupFile = webAppLookup hashFileIfExists "static"
    , ssMkRedirect  = defaultMkRedirect
    , ssGetMimeType = return . defaultMimeTypeByExt . fileName
    , ssMaxAge  = MaxAgeForever
    , ssListing = Nothing
    , ssIndices = []
    , ssRedirectToIndex = False
    , ssUseHash = True
    }

defaultFileServerSettings :: StaticSettings
defaultFileServerSettings = StaticSettings
    { ssLookupFile = fileSystemLookup "static"
    , ssMkRedirect = defaultMkRedirect
    , ssGetMimeType = return . defaultMimeTypeByExt . fileName
    , ssMaxAge = MaxAgeSeconds $ 60 * 60
    , ssListing = Just defaultListing
    , ssIndices = map unsafeToPiece ["index.html", "index.htm"]
    , ssRedirectToIndex = False
    , ssUseHash = False
    }

webAppSettingsWithLookup :: FilePath -> ETagLookup -> StaticSettings
webAppSettingsWithLookup dir etagLookup =
  defaultWebAppSettings { ssLookupFile = webAppLookup etagLookup dir}

fileHelper :: ETagLookup
           -> FilePath -- ^ file location
           -> Piece -- ^ file name
           -> IO LookupResult
fileHelper hashFunc fp name = do
    efs <- try $ getFileStatus $ F.encodeString fp
    case efs of
        Left (_ :: SomeException) -> return LRNotFound
        Right fs -> return $ LRFile File
            { fileGetSize = fromIntegral $ fileSize fs
            , fileToResponse = \s h -> W.ResponseFile s h (F.encodeString fp) Nothing
            , fileName = name
            , fileGetHash = hashFunc fp
            , fileGetModified = Just $ modificationTime fs
            }

type ETagLookup = (FilePath -> IO (Maybe ByteString))

webAppLookup :: ETagLookup -> FilePath -> Pieces -> IO LookupResult
webAppLookup cachedLookupHash prefix pieces =
    fileHelper cachedLookupHash fp $ last pieces
  where
    fp = pathFromPieces prefix pieces

defaultFileSystemHash :: ETagLookup
defaultFileSystemHash fp = fmap Just $ hashFile fp

-- FIXME replace lazy IO with enumerators
-- FIXME let's use a dictionary to cache these values?
hashFile :: FilePath -> IO ByteString -- FIXME use crypto-conduit
hashFile fp = do
    l <- L.readFile $ F.encodeString fp
    return $ runHashL l

hashFileIfExists :: ETagLookup
hashFileIfExists fp = do
    fe <- F.isFile fp
    if fe
      then return Nothing
      else defaultFileSystemHash fp

fileSystemLookup :: FilePath -> Pieces -> IO LookupResult
fileSystemLookup = fileSystemLookupHash defaultFileSystemHash

filePathToPiece :: FilePath -> Piece
filePathToPiece = unsafeToPiece . either id id . F.toText

isVisible :: FilePath -> Bool
isVisible =
    go . F.encodeString
  where
    go ('.':_) = False
    go "" = False
    go _ = True

fileSystemLookupHash :: ETagLookup
                     -> FilePath -> Pieces -> IO LookupResult
fileSystemLookupHash hashFunc prefix pieces = do
    let fp = pathFromPieces prefix pieces
    fe <- F.isFile fp
    if fe
        then fileHelper hashFunc fp $ last pieces
        else do
            de <- F.isDirectory fp
            if de
                then do
                    entries' <- fmap (filter isVisible) $ F.listDirectory fp
                    entries <- forM entries' $ \fp' -> do
                        let name =
                                case toPiece $ either id id $ F.toText $ F.filename fp' of
                                    Just p -> p
                                    Nothing -> error "fileSystemLookupHash: FIXME"
                        mfile' <- fileHelper hashFunc fp' name
                        return $ case mfile' of
                            LRNotFound -> Left $ Folder (filePathToPiece $ F.filename fp') []
                            LRFolder f -> Left f
                            LRFile f -> Right f
                    return $ LRFolder $ Folder (error "Network.Wai.Application.Static.fileSystemLookup") entries
                else return LRNotFound
