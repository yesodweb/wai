{-# LANGUAGE RecordWildCards, CPP #-}

module Network.Wai.Handler.Warp.FileInfoCache (
  -- * Types
    FileInfo(..)
  -- * Starter
  , fileCacheInit
  ) where

import Control.Exception as E
import Control.Reaper
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Network.HTTP.Date
import System.PosixCompat.Files

#if WINDOWS
import Data.Time (UTCTime)
import Network.Wai.Handler.Warp.Date (uToH)
#endif

data FileInfo = FileInfo {
    fileInfoName :: !FilePath
  , fileInfoSize :: !Integer
  , fileInfoTime :: !HTTPDate   -- ^ Modification time
  , fileInfoDate :: !ByteString -- ^ Modification time in the GMT format
  } deriving (Eq, Show)

data Entry = Negative | Positive FileInfo
type Cache = HashMap FilePath Entry
type FileCache = Reaper Cache (FilePath,Entry)

fileInfo :: FileCache -> FilePath -> IO FileInfo
fileInfo reaper@Reaper{..} path = do
    cache <- reaperRead
    case M.lookup path cache of
        Just Negative     -> throwIO (userError "fileInfo")
        Just (Positive x) -> return x
        Nothing           -> register reaper path `E.onException` negative reaper path

register :: FileCache -> FilePath -> IO FileInfo
register reaper path = do
    fs <- getFileStatus path -- file access
    let regular = not (isDirectory fs)
        readable = fileMode fs `intersectFileModes` ownerReadMode /= 0
    if regular && readable then
        positive reaper fs path
      else
        throwIO (userError "go to negative")

positive :: FileCache -> FileStatus -> FilePath -> IO FileInfo
positive Reaper{..} fs path = do
    reaperAdd (path,entry)
    return info
  where
    info = FileInfo {
        fileInfoName = path
      , fileInfoSize = size fs
      , fileInfoTime = time
      , fileInfoDate = formatHTTPDate time
      }
    size = fromIntegral . fileSize
    time = epochTimeToHTTPDate (modificationTime fs)
    entry = Positive info

negative :: FileCache -> FilePath -> IO FileInfo
negative Reaper{..} path = do
    reaperAdd (path,Negative)
    throwIO (userError "fileInfo")

----------------------------------------------------------------

fileCacheInit :: IO (FilePath -> IO FileInfo)
fileCacheInit = mkReaper settings >>= return . fileInfo
  where
    settings = defaultReaperSettings {
        reaperAction = override
      , reaperDelay  = 10000000 -- 10 seconds -- hard-coding: fixme
      , reaperCons   = uncurry M.insert
      , reaperNull   = M.null
      , reaperEmpty  = M.empty
      }

override :: Cache -> IO (Cache -> Cache)
override _ = return $ const M.empty
