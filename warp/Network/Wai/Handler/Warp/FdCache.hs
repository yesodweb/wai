{-# LANGUAGE BangPatterns, FlexibleInstances, CPP #-}

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
import Control.Concurrent (threadDelay)
import Control.Exception (bracket, mask_)
import Data.Hashable (hash)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef)
import Network.Wai.Handler.Warp.MultiMap
import Network.Wai.Handler.Warp.Thread
import System.Posix.IO (openFd, OpenFileFlags(..), defaultFileFlags, OpenMode(ReadOnly), closeFd)
import System.Posix.Types (Fd)

----------------------------------------------------------------

data Status = Active | Inactive

newtype MutableStatus = MutableStatus (IORef Status)

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
newtype MutableFdCache = MutableFdCache (IORef FdCache)

fdCache :: MutableFdCache -> IO FdCache
fdCache (MutableFdCache ref) = readIORef ref

swapWithNew :: IORef FdCache -> IO FdCache
swapWithNew ref = atomicModifyIORef ref (\t -> (empty, t))

update :: MutableFdCache -> (FdCache -> FdCache) -> IO ()
update (MutableFdCache ref) = update' ref

update' :: IORef FdCache -> (FdCache -> FdCache) -> IO ()
update' ref f = do
    !_  <- atomicModifyIORef ref $ \t -> let !new = f t in (new, ())
    return ()

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

withFdCache :: Int -> (Maybe MutableFdCache -> IO a) -> IO a
withFdCache duration action = bracket (initialize duration)
                                      terminate
                                      action

----------------------------------------------------------------

initialize :: Int -> IO (Maybe MutableFdCache)
initialize 0 = return Nothing
initialize duration = do
    ref' <- forkIOwithBreakableForever empty $ \ref -> do
        threadDelay duration
        clean ref
    return (Just (MutableFdCache ref'))

clean :: IORef FdCache -> IO ()
clean ref = do
    old <- swapWithNew ref
    new <- pruneWith old prune
    update' ref (merge new)

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
terminate (Just (MutableFdCache ref)) = mask_ $ do
    !t <- breakForever ref
    mapM_ closeIt $ toList t
  where
    closeIt (_, FdEntry _ fd _) = closeFd fd

----------------------------------------------------------------

getFd :: MutableFdCache -> FilePath -> IO (Fd, Refresh)
getFd mfc path = look mfc path key >>= getFd'
  where
    key = hash path
    getFd' Nothing = do
        ent@(FdEntry _ fd mst) <- newFdEntry path
        update mfc (insert key ent)
        return (fd, refresh mst)
    getFd' (Just (FdEntry _ fd mst)) = do
        refresh mst
        return (fd, refresh mst)
#endif
