{-# LANGUAGE RecordWildCards, CPP #-}

module Network.Wai.Handler.Warp.FileInfoCache (
  -- * Types
    FileInfo(..)
  -- * Functions
  , withFileInfoCache
  ) where

import Control.Exception as E
import Control.Monad (void)
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

----------------------------------------------------------------

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

withFileInfoCache :: Int -> ((FilePath -> IO FileInfo) -> IO a) -> IO a
withFileInfoCache 0        action = action getInfo
withFileInfoCache duration action = E.bracket (initialize duration)
                                              terminate
                                              (\c -> action (getAndRegisterInfo c))

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
