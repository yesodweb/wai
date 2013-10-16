{-# LANGUAGE BangPatterns, FlexibleInstances, CPP #-}

#ifndef SENDFILEFD
module Network.Wai.Handler.Warp.FdCache (
    withFdCache
  , MutableFdCacheSet
  ) where

data MutableFdCacheSet = MutableFdCacheSet

withFdCache :: Int -> (Maybe MutableFdCacheSet -> IO a) -> IO a
withFdCache _ f = f Nothing
#else
module Network.Wai.Handler.Warp.FdCache (
    withFdCache
  , getFd
  , MutableFdCacheSet
  ) where

import Control.Applicative ((<$>), (<*>))
import Control.Concurrent (forkIO, threadDelay, myThreadId, threadCapability, getNumCapabilities)
import Control.Exception (bracket)
import Control.Monad (replicateM, void)
import Data.Array (Array, (!), listArray, elems)
import Data.Hashable (hash)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, atomicModifyIORef)
import Network.Wai.Handler.Warp.MultiMap
import System.Posix.IO (openFd, defaultFileFlags, OpenMode(ReadOnly), closeFd)
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
              <$> openFd path ReadOnly Nothing defaultFileFlags
              <*> newActiveStatus

----------------------------------------------------------------

type Hash = Int
type FdCache = MMap Hash FdEntry
newtype MutableFdCache = MutableFdCache (IORef FdCache)
newtype MutableFdCacheSet = MutableFdCacheSet (Array Int MutableFdCache)

newMutableFdCache :: IO MutableFdCache
newMutableFdCache = MutableFdCache <$> newIORef empty

fdCache :: MutableFdCache -> IO FdCache
fdCache (MutableFdCache ref) = readIORef ref

swapWithNew :: MutableFdCache -> IO FdCache
swapWithNew (MutableFdCache ref) = atomicModifyIORef ref (\t -> (empty, t))

update :: MutableFdCache -> (FdCache -> FdCache) -> IO ()
update (MutableFdCache ref) f = do
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

withFdCache :: Int -> (Maybe MutableFdCacheSet -> IO a) -> IO a
withFdCache duration action = bracket (initialize duration)
                                      terminate
                                      action

----------------------------------------------------------------

initialize :: Int -> IO (Maybe MutableFdCacheSet)
initialize 0 = return Nothing
initialize duration = do
    mfcset <- create
    -- FIXME: how to stop this thread?
    void . forkIO $ cleanLoop duration mfcset
    return (Just mfcset)

create :: IO MutableFdCacheSet
create = do
    n <- getNumCapabilities
    mfcs <- replicateM n newMutableFdCache
    let mfcset = listArray (0,n-1) mfcs
    return $ MutableFdCacheSet mfcset

cleanLoop :: Int -> MutableFdCacheSet -> IO ()
cleanLoop duration (MutableFdCacheSet mfcs) = loop
 where
   loop = do
       mapM_ clean $ elems mfcs
       threadDelay duration
       loop

clean :: MutableFdCache -> IO ()
clean mfc = do
    old <- swapWithNew mfc
    new <- pruneWith old prune
    update mfc (merge new)

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

terminate :: Maybe MutableFdCacheSet -> IO ()
terminate Nothing = return ()
terminate (Just (MutableFdCacheSet mfcs)) = mapM_ cleanup $ elems mfcs

cleanup :: MutableFdCache -> IO ()
cleanup (MutableFdCache mfc) = readIORef mfc >>= mapM_ closeIt . toList
  where
    closeIt (_, FdEntry _ fd _) = closeFd fd

----------------------------------------------------------------

getFd :: MutableFdCacheSet -> FilePath -> IO (Fd, Refresh)
getFd mfcs path = do
    mfc <- myMutableFdCache mfcs
    look mfc path key >>= getFd' mfc
  where
    key = hash path
    getFd' mfc Nothing = do
        ent@(FdEntry _ fd mst) <- newFdEntry path
        update mfc (insert key ent)
        return (fd, refresh mst)
    getFd' _ (Just (FdEntry _ fd mst)) = do
        refresh mst
        return (fd, refresh mst)
----------------------------------------------------------------

myMutableFdCache :: MutableFdCacheSet -> IO MutableFdCache
myMutableFdCache (MutableFdCacheSet mfcs) = do
    (i, _) <- myThreadId >>= threadCapability
    return $ mfcs ! i
#endif
