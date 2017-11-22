{-# LANGUAGE BangPatterns, CPP #-}

-- | File descriptor cache to avoid locks in kernel.

module Network.Wai.Handler.Warp.FdCache (
    withFdCache
  , Fd
  , Refresh
#ifndef WINDOWS
  , openFile
  , closeFile
  , setFileCloseOnExec
#endif
  ) where

#ifndef WINDOWS
import Control.Exception (bracket)
import Control.Reaper
import Data.IORef
import Network.Wai.Handler.Warp.MultiMap
import System.Posix.IO (openFd, OpenFileFlags(..), defaultFileFlags, OpenMode(ReadOnly), closeFd, FdOption(CloseOnExec), setFdOption)
#endif
import System.Posix.Types (Fd)

----------------------------------------------------------------

type Hash = Int

-- | An action to activate a Fd cache entry.
type Refresh = IO ()

getFdNothing :: Hash -> FilePath -> IO (Maybe Fd, Refresh)
getFdNothing _ _ = return (Nothing, return ())

----------------------------------------------------------------

-- | Creating 'MutableFdCache' and executing the action in the second
--   argument. The first argument is a cache duration in second.
withFdCache :: Int -> ((Hash -> FilePath -> IO (Maybe Fd, Refresh)) -> IO a) -> IO a
#ifdef WINDOWS
withFdCache _        action = action getFdNothing
#else
withFdCache 0        action = action getFdNothing
withFdCache duration action = bracket (initialize duration)
                                      terminate
                                      (action . getFd)

----------------------------------------------------------------

data Status = Active | Inactive

newtype MutableStatus = MutableStatus (IORef Status)

status :: MutableStatus -> IO Status
status (MutableStatus ref) = readIORef ref

newActiveStatus :: IO MutableStatus
newActiveStatus = MutableStatus <$> newIORef Active

refresh :: MutableStatus -> Refresh
refresh (MutableStatus ref) = writeIORef ref Active

inactive :: MutableStatus -> IO ()
inactive (MutableStatus ref) = writeIORef ref Inactive

----------------------------------------------------------------

data FdEntry = FdEntry !FilePath !Fd !MutableStatus

openFile :: FilePath -> IO Fd
openFile path = do
    fd <- openFd path ReadOnly Nothing defaultFileFlags{nonBlock=False}
    setFileCloseOnExec fd
    return fd

closeFile :: Fd -> IO ()
closeFile = closeFd

newFdEntry :: FilePath -> IO FdEntry
newFdEntry path = FdEntry path <$> openFile path <*> newActiveStatus

setFileCloseOnExec :: Fd -> IO ()
setFileCloseOnExec fd = setFdOption fd CloseOnExec True

----------------------------------------------------------------

type FdCache = MMap FdEntry

-- | Mutable Fd cacher.
newtype MutableFdCache = MutableFdCache (Reaper FdCache (Hash, FdEntry))

fdCache :: MutableFdCache -> IO FdCache
fdCache (MutableFdCache reaper) = reaperRead reaper

look :: MutableFdCache -> FilePath -> Hash -> IO (Maybe FdEntry)
look mfc path key = searchWith key check <$> fdCache mfc
  where
    check (FdEntry path' _ _) = path == path'

----------------------------------------------------------------

-- The first argument is a cache duration in second.
initialize :: Int -> IO MutableFdCache
initialize duration = MutableFdCache <$> mkReaper settings
  where
    settings = defaultReaperSettings {
        reaperAction = clean
      , reaperDelay = duration
      , reaperCons = uncurry insert
      , reaperNull = isEmpty
      , reaperEmpty = empty
      }

clean :: FdCache -> IO (FdCache -> FdCache)
clean old = do
    new <- pruneWith old prune
    return $ merge new
  where
    prune (FdEntry _ fd mst) = status mst >>= act
      where
        act Active   = inactive mst >> return True
        act Inactive = closeFd fd   >> return False

----------------------------------------------------------------

terminate :: MutableFdCache -> IO ()
terminate (MutableFdCache reaper) = do
    !t <- reaperStop reaper
    mapM_ closeIt $ toList t
  where
    closeIt (FdEntry _ fd _) = closeFd fd

----------------------------------------------------------------

-- | Getting 'Fd' and 'Refresh' from the mutable Fd cacher.
getFd :: MutableFdCache -> Hash -> FilePath -> IO (Maybe Fd, Refresh)
getFd mfc@(MutableFdCache reaper) h path = look mfc path h >>= get
  where
    get Nothing = do
        ent@(FdEntry _ fd mst) <- newFdEntry path
        reaperAdd reaper (h, ent)
        return (Just fd, refresh mst)
    get (Just (FdEntry _ fd mst)) = do
        refresh mst
        return (Just fd, refresh mst)
#endif
