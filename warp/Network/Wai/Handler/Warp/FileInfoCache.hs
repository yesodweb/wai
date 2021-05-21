{-# LANGUAGE RecordWildCards, CPP #-}

module Network.Wai.Handler.Warp.FileInfoCache (
    FileInfo(..)
  , withFileInfoCache
  , getInfo -- test purpose only
  ) where

import qualified UnliftIO (onException, bracket, throwIO)
import Control.Reaper
import Network.HTTP.Date
import System.PosixCompat.Files

import Network.Wai.Handler.Warp.HashMap (HashMap)
import qualified Network.Wai.Handler.Warp.HashMap as M
import Network.Wai.Handler.Warp.Imports

----------------------------------------------------------------

-- | File information.
data FileInfo = FileInfo {
    fileInfoName :: !FilePath
  , fileInfoSize :: !Integer
  , fileInfoTime :: HTTPDate   -- ^ Modification time
  , fileInfoDate :: ByteString -- ^ Modification time in the GMT format
  } deriving (Eq, Show)

data Entry = Negative | Positive FileInfo
type Cache = HashMap Entry
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
        UnliftIO.throwIO (userError "FileInfoCache:getInfo")

getInfoNaive :: FilePath -> IO FileInfo
getInfoNaive = getInfo

----------------------------------------------------------------

getAndRegisterInfo :: FileInfoCache -> FilePath -> IO FileInfo
getAndRegisterInfo reaper@Reaper{..} path = do
    cache <- reaperRead
    case M.lookup path cache of
        Just Negative     -> UnliftIO.throwIO (userError "FileInfoCache:getAndRegisterInfo")
        Just (Positive x) -> return x
        Nothing           -> positive reaper path
                               `UnliftIO.onException` negative reaper path

positive :: FileInfoCache -> FilePath -> IO FileInfo
positive Reaper{..} path = do
    info <- getInfo path
    reaperAdd (path, Positive info)
    return info

negative :: FileInfoCache -> FilePath -> IO FileInfo
negative Reaper{..} path = do
    reaperAdd (path, Negative)
    UnliftIO.throwIO (userError "FileInfoCache:negative")

----------------------------------------------------------------

-- | Creating a file information cache
--   and executing the action in the second argument.
--   The first argument is a cache duration in second.
withFileInfoCache :: Int
                  -> ((FilePath -> IO FileInfo) -> IO a)
                  -> IO a
withFileInfoCache 0        action = action getInfoNaive
withFileInfoCache duration action =
    UnliftIO.bracket
      (initialize duration)
      terminate
      (action . getAndRegisterInfo)

initialize :: Int -> IO FileInfoCache
initialize duration = mkReaper settings
  where
    settings = defaultReaperSettings {
        reaperAction = override
      , reaperDelay  = duration
      , reaperCons   = \(path,v) -> M.insert path v
      , reaperNull   = M.isEmpty
      , reaperEmpty  = M.empty
      }

override :: Cache -> IO (Cache -> Cache)
override _ = return $ const M.empty

terminate :: FileInfoCache -> IO ()
terminate x = void $ reaperStop x
