{-# LANGUAGE RecordWildCards, CPP #-}

module Network.Wai.Handler.Warp.FileInfoCache (
    FileInfo(..)
  , Hash
  , withFileInfoCache
  , getInfo -- test purpose only
  ) where

import Control.Exception as E
import Control.Reaper
import Network.HTTP.Date
import System.PosixCompat.Files

import Network.Wai.Handler.Warp.HashMap (HashMap)
import qualified Network.Wai.Handler.Warp.HashMap as M
import Network.Wai.Handler.Warp.Imports

----------------------------------------------------------------

type Hash = Int

-- | File information.
data FileInfo = FileInfo {
    fileInfoName :: !FilePath
  , fileInfoSize :: !Integer
  , fileInfoTime :: HTTPDate   -- ^ Modification time
  , fileInfoDate :: ByteString -- ^ Modification time in the GMT format
  } deriving (Eq, Show)

data Entry = Negative | Positive FileInfo
type Cache = HashMap FilePath Entry
type FileInfoCache = Reaper Cache (Int,FilePath,Entry)

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

getInfoNaive :: Hash -> FilePath -> IO FileInfo
getInfoNaive _ = getInfo

----------------------------------------------------------------

getAndRegisterInfo :: FileInfoCache -> Hash -> FilePath -> IO FileInfo
getAndRegisterInfo reaper@Reaper{..} h path = do
    cache <- reaperRead
    case M.lookup h path cache of
        Just Negative     -> throwIO (userError "FileInfoCache:getAndRegisterInfo")
        Just (Positive x) -> return x
        Nothing           -> positive reaper h path
                               `E.onException` negative reaper h path

positive :: FileInfoCache -> Hash -> FilePath -> IO FileInfo
positive Reaper{..} h path = do
    info <- getInfo path
    reaperAdd (h, path, Positive info)
    return info

negative :: FileInfoCache -> Hash -> FilePath -> IO FileInfo
negative Reaper{..} h path = do
    reaperAdd (h, path,Negative)
    throwIO (userError "FileInfoCache:negative")

----------------------------------------------------------------

-- | Creating a file information cache
--   and executing the action in the second argument.
--   The first argument is a cache duration in second.
withFileInfoCache :: Int
                  -> ((Hash -> FilePath -> IO FileInfo) -> IO a)
                  -> IO a
withFileInfoCache 0        action = action getInfoNaive
withFileInfoCache duration action =
    E.bracket (initialize duration)
              terminate
              (action . getAndRegisterInfo)

initialize :: Hash -> IO FileInfoCache
initialize duration = mkReaper settings
  where
    settings = defaultReaperSettings {
        reaperAction = override
      , reaperDelay  = duration
      , reaperCons   = \(h,k,v) -> M.insert h k v
      , reaperNull   = M.null
      , reaperEmpty  = M.empty
      }

override :: Cache -> IO (Cache -> Cache)
override _ = return $ const M.empty

terminate :: FileInfoCache -> IO ()
terminate x = void $ reaperStop x
