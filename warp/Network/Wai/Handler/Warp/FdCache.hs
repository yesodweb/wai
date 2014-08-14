{-# LANGUAGE BangPatterns, CPP #-}

-- | File descriptor cache to avoid locks in kernel.

#ifndef SENDFILEFD
module Network.Wai.Handler.Warp.FdCache (
    withFdCache
  , MutableFdCache
  , Refresh
  ) where

type Refresh = IO ()
data MutableFdCache = MutableFdCache

withFdCache :: Int -> (Maybe MutableFdCache -> IO a) -> IO a
withFdCache _ f = f Nothing
#else
module Network.Wai.Handler.Warp.FdCache (
    withFdCache
  , getFd
  , MutableFdCache
  , Refresh
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception (bracket)
import Data.Hashable (hash)
import Network.Wai.Handler.Warp.IORef
import Network.Wai.Handler.Warp.MultiMap
import System.Posix.IO (openFd, OpenFileFlags(..), defaultFileFlags, OpenMode(ReadOnly), closeFd)
import System.Posix.Types (Fd)
import Control.Reaper

----------------------------------------------------------------

data Status = Active | Inactive

newtype MutableStatus = MutableStatus (IORef Status)

-- | An action to activate a Fd cache entry.
type Refresh = IO ()

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

newFdEntry :: FilePath -> IO FdEntry
newFdEntry path = FdEntry path
              <$> openFd path ReadOnly Nothing defaultFileFlags{nonBlock=True}
              <*> newActiveStatus

----------------------------------------------------------------

type Hash = Int
type FdCache = MMap Hash FdEntry

-- | Mutable Fd cacher.
type MutableFdCache = Reaper FdCache (Hash, FdEntry)

fdCache :: MutableFdCache -> IO FdCache
fdCache mfc = reaperRead mfc

look :: MutableFdCache -> FilePath -> Hash -> IO (Maybe FdEntry)
look mfc path key = searchWith key check <$> fdCache mfc
  where
    check (One ent@(FdEntry path' _ _))
      | path == path' = Just ent
      | otherwise     = Nothing
    check (Tom ent@(FdEntry path' _ _) vs)
      | path == path' = Just ent
      | otherwise     = check vs

----------------------------------------------------------------

-- | Creating 'MutableFdCache' and executing the action in the second
--   argument. The first argument is a cache duration in second.
withFdCache :: Int -> (Maybe MutableFdCache -> IO a) -> IO a
withFdCache duration action = bracket (initialize duration)
                                      terminate
                                      action

----------------------------------------------------------------

-- The first argument is a cache duration in second.
initialize :: Int -> IO (Maybe MutableFdCache)
initialize 0 = return Nothing
initialize duration = Just <$> mkReaper defaultReaperSettings
    { reaperAction = clean
    , reaperDelay = duration
    , reaperCons = uncurry insert
    , reaperNull = isEmpty
    , reaperEmpty = empty
    }

clean :: FdCache -> IO (FdCache -> FdCache)
clean old = do
    new <- pruneWith old prune
    return $ merge new

prune :: t -> Some FdEntry -> IO [(t, Some FdEntry)]
prune k v@(One (FdEntry _ fd mst)) = status mst >>= prune'
  where
    prune' Active   = inactive mst >> return [(k,v)]
    prune' Inactive = closeFd fd   >> return []
prune k (Tom ent@(FdEntry _ fd mst) vs) = status mst >>= prune'
  where
    prune' Active = do
        inactive mst
        zs <- prune k vs
        case zs of
            []        -> return [(k,One ent)]
            [(_,zvs)] -> return [(k,Tom ent zvs)]
            _         -> error "prune"
    prune' Inactive = closeFd fd >> prune k vs

----------------------------------------------------------------

terminate :: Maybe MutableFdCache -> IO ()
terminate Nothing = return ()
terminate (Just mfc) = do
    !t <- reaperStop mfc
    mapM_ closeIt $ toList t
  where
    closeIt (_, FdEntry _ fd _) = closeFd fd

----------------------------------------------------------------

-- | Getting 'Fd' and 'Refresh' from the mutable Fd cacher.
getFd :: MutableFdCache -> FilePath -> IO (Fd, Refresh)
getFd mfc path = look mfc path key >>= getFd'
  where
    key = hash path
    getFd' Nothing = do
        ent@(FdEntry _ fd mst) <- newFdEntry path
        reaperAdd mfc (key, ent)
        return (fd, refresh mst)
    getFd' (Just (FdEntry _ fd mst)) = do
        refresh mst
        return (fd, refresh mst)
#endif
