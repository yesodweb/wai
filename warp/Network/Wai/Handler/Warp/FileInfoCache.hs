{-# LANGUAGE RecordWildCards, CPP #-}

module Network.Wai.Handler.Warp.FileInfoCache (
    FileInfo(..)
  , withFileInfoCache
  , getInfo -- test purpose only
  ) where

import Control.Exception as E
import Control.Monad (void)
import Control.Reaper
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import Network.HTTP.Date
import System.PosixCompat.Files

----------------------------------------------------------------

-- | File information.
data FileInfo = FileInfo {
    fileInfoName :: !FilePath
  , fileInfoSize :: !Integer
  , fileInfoTime :: HTTPDate   -- ^ Modification time
  , fileInfoDate :: ByteString -- ^ Modification time in the GMT format
  } deriving (Eq, Show)

data Entry = Negative | Positive FileInfo
type Cache = HashMap FilePath Entry
type FileInfoCache = Reaper Cache (FilePath,Entry)

----------------------------------------------------------------

-- | Getting the file information corresponding to the file.
getInfo :: FilePath -> IO FileInfo
getInfo path = do
    fs <- getFileStatus path -- file access
    let regular = not (isDirectory fs)
        readable = fileMode fs `intersectFileModes` ownerReadMode /= 0
    if regular && readable then do
        let time = epochTimeToHTTPDate $ modificationTime fs
            date = formatHTTPDate time
            size = fromIntegral $ fileSize fs
            info = FileInfo {
                fileInfoName = path
              , fileInfoSize = size
              , fileInfoTime = time
              , fileInfoDate = date
              }
        return info
      else
        throwIO (userError "FileInfoCache:getInfo")

----------------------------------------------------------------

getAndRegisterInfo :: FileInfoCache -> FilePath -> IO FileInfo
getAndRegisterInfo reaper@Reaper{..} path = do
    cache <- reaperRead
    case M.lookup path cache of
        Just Negative     -> throwIO (userError "FileInfoCache:getAndRegisterInfo")
        Just (Positive x) -> return x
        Nothing           -> positive reaper path `E.onException` negative reaper path

positive :: FileInfoCache -> FilePath -> IO FileInfo
positive Reaper{..} path = do
    info <- getInfo path
    reaperAdd (path, Positive info)
    return info

negative :: FileInfoCache -> FilePath -> IO FileInfo
negative Reaper{..} path = do
    reaperAdd (path,Negative)
    throwIO (userError "FileInfoCache:negative")

----------------------------------------------------------------

-- | Creating a file information cache
--   and executing the action in the second argument.
--   The first argument is a cache duration in second.
withFileInfoCache :: Int -> ((FilePath -> IO FileInfo) -> IO a) -> IO a
withFileInfoCache 0        action = action getInfo
withFileInfoCache duration action = E.bracket (initialize duration)
                                              terminate
                                              (action . getAndRegisterInfo)

initialize :: Int -> IO FileInfoCache
initialize duration = mkReaper settings
  where
    settings = defaultReaperSettings {
        reaperAction = override
      , reaperDelay  = duration * 1000000
      , reaperCons   = uncurry M.insert
      , reaperNull   = M.null
      , reaperEmpty  = M.empty
      }

override :: Cache -> IO (Cache -> Cache)
override _ = return $ const M.empty

terminate :: FileInfoCache -> IO ()
terminate x = void $ reaperStop x
