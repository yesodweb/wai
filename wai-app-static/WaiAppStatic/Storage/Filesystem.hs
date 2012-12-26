{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Access files on the filesystem.
module WaiAppStatic.Storage.Filesystem
    ( -- * Types
      ETagLookup
      -- * Settings
    , defaultWebAppSettings
    , defaultFileServerSettings
    , webAppSettingsWithLookup
    ) where

import WaiAppStatic.Types
import Prelude hiding (FilePath)
import Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as F
import qualified Filesystem as F
import Data.List (foldl')
import Control.Monad (forM)
import Util
import Data.ByteString (ByteString)
import Control.Exception (SomeException, try)
import qualified Network.Wai as W
import WaiAppStatic.Listing
import Network.Mime
import System.PosixCompat.Files (fileSize, getFileStatus, modificationTime, isRegularFile)
import Data.Maybe (catMaybes)
import qualified Crypto.Conduit
import Data.Serialize (encode)
import Crypto.Hash.MD5 (MD5)
import qualified Data.ByteString.Base64 as B64

-- | Construct a new path from a root and some @Pieces@.
pathFromPieces :: FilePath -> Pieces -> FilePath
pathFromPieces = foldl' (\fp p -> fp </> F.fromText (fromPiece p))

-- | Settings optimized for a web application. Files will have aggressive
-- caching applied and hashes calculated, and indices and listings are disabled.
defaultWebAppSettings :: FilePath -- ^ root folder to serve from
                      -> StaticSettings
defaultWebAppSettings root = StaticSettings
    { ssLookupFile = webAppLookup hashFileIfExists root
    , ssMkRedirect  = defaultMkRedirect
    , ssGetMimeType = return . defaultMimeLookup . fromPiece . fileName
    , ssMaxAge  = MaxAgeForever
    , ssListing = Nothing
    , ssIndices = []
    , ssRedirectToIndex = False
    , ssUseHash = True
    }

-- | Settings optimized for a file server. More conservative caching will be
-- applied, and indices and listings are enabled.
defaultFileServerSettings :: FilePath -- ^ root folder to serve from
                          -> StaticSettings
defaultFileServerSettings root = StaticSettings
    { ssLookupFile = fileSystemLookup (fmap Just . hashFile) root
    , ssMkRedirect = defaultMkRedirect
    , ssGetMimeType = return . defaultMimeLookup . fromPiece . fileName
    , ssMaxAge = NoMaxAge
    , ssListing = Just defaultListing
    , ssIndices = map unsafeToPiece ["index.html", "index.htm"]
    , ssRedirectToIndex = False
    , ssUseHash = False
    }

-- | Same as @defaultWebAppSettings@, but additionally uses a specialized
-- @ETagLookup@ in place of the standard one. This can allow you to cache your
-- hash values, or even precompute them.
webAppSettingsWithLookup :: FilePath -- ^ root folder to serve from
                         -> ETagLookup
                         -> StaticSettings
webAppSettingsWithLookup dir etagLookup =
  (defaultWebAppSettings dir) { ssLookupFile = webAppLookup etagLookup dir}

-- | Convenience wrapper for @fileHelper@.
fileHelperLR :: ETagLookup
             -> FilePath -- ^ file location
             -> Piece -- ^ file name
             -> IO LookupResult
fileHelperLR a b c = fmap (maybe LRNotFound LRFile) $ fileHelper a b c

-- | Attempt to load up a @File@ from the given path.
fileHelper :: ETagLookup
           -> FilePath -- ^ file location
           -> Piece -- ^ file name
           -> IO (Maybe File)
fileHelper hashFunc fp name = do
    efs <- try $ getFileStatus $ F.encodeString fp
    case efs of
        Left (_ :: SomeException) -> return Nothing
        Right fs | isRegularFile fs -> return $ Just File
            { fileGetSize = fromIntegral $ fileSize fs
            , fileToResponse = \s h -> W.ResponseFile s h (F.encodeString fp) Nothing
            , fileName = name
            , fileGetHash = hashFunc fp
            , fileGetModified = Just $ modificationTime fs
            }
        Right _ -> return Nothing

-- | How to calculate etags. Can perform filesystem reads on each call, or use
-- some caching mechanism.
type ETagLookup = FilePath -> IO (Maybe ByteString)

-- | More efficient than @fileSystemLookup@ as it only concerns itself with
-- finding files, not folders.
webAppLookup :: ETagLookup -> FilePath -> Pieces -> IO LookupResult
webAppLookup hashFunc prefix pieces =
    fileHelperLR hashFunc fp lastPiece
  where
    fp = pathFromPieces prefix pieces
    lastPiece
        | null pieces = unsafeToPiece ""
        | otherwise = last pieces

-- | MD5 hash and base64-encode the file contents. Does not check if the file
-- exists.
hashFile :: FilePath -> IO ByteString
hashFile fp = do
    h <- Crypto.Conduit.hashFile (F.encodeString fp)
    return $ B64.encode $ encode (h :: MD5)

hashFileIfExists :: ETagLookup
hashFileIfExists fp = do
    res <- try $ hashFile fp
    return $ case res of
        Left (_ :: SomeException) -> Nothing
        Right x -> Just x

isVisible :: FilePath -> Bool
isVisible =
    go . F.encodeString . F.filename
  where
    go ('.':_) = False
    go "" = False
    go _ = True

-- | Get a proper @LookupResult@, checking if the path is a file or folder.
-- Compare with @webAppLookup@, which only deals with files.
fileSystemLookup :: ETagLookup
                 -> FilePath -> Pieces -> IO LookupResult
fileSystemLookup hashFunc prefix pieces = do
    let fp = pathFromPieces prefix pieces
    fe <- F.isFile fp
    if fe
        then fileHelperLR hashFunc fp lastPiece
        else do
            de <- F.isDirectory fp
            if de
                then do
                    entries' <- fmap (filter isVisible) $ F.listDirectory fp
                    entries <- forM entries' $ \fp' -> do
                        let name = unsafeToPiece $ either id id $ F.toText $ F.filename fp'
                        de' <- F.isDirectory fp'
                        if de'
                            then return $ Just $ Left name
                            else do
                                mfile <- fileHelper hashFunc fp' name
                                case mfile of
                                    Nothing -> return Nothing
                                    Just file -> return $ Just $ Right file
                    return $ LRFolder $ Folder $ catMaybes entries
                else return LRNotFound
  where
    lastPiece
        | null pieces = unsafeToPiece ""
        | otherwise = last pieces
