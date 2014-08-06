{-# LANGUAGE RecordWildCards    #-}

-- | This module provides the ability to create reapers: dedicated cleanup
-- threads. These threads will automatically spawn and die based on the
-- presence of a workload to process on.
module Control.Reaper (
      -- * Settings
      ReaperSettings
    , defaultReaperSettings
      -- * Accessors
    , reaperAction
    , reaperDelay
    , reaperCons
    , reaperNull
    , reaperEmpty
      -- * Type
    , Reaper(..)
      -- * Creation
    , mkReaper
      -- * Helper
    , mkListAction
    ) where

import Control.AutoUpdate.Util (atomicModifyIORef')
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (mask_)
import Control.Monad (join, void)
import Data.IORef (IORef, newIORef, readIORef)

-- | Settings for creating a reaper. This type has two parameters:
-- @workload@ gives the entire workload, whereas @item@ gives an
-- individual piece of the queue. A common approach is to have @workload@
-- be a list of @item@s. This is encouraged by 'defaultReaperSettings' and
-- 'mkListAction'.
--
-- Since 0.1.1
data ReaperSettings workload item = ReaperSettings
    { reaperAction :: workload -> IO (workload -> workload)
    -- ^ The action to perform on a workload. The result of this is a
    -- \"workload modifying\" function. In the common case of using lists,
    -- the result should be a difference list that prepends the remaining
    -- workload to the temporary workload. For help with setting up such
    -- an action, see 'mkListAction'.
    --
    -- Default: do nothing with the workload, and then prepend it to the
    -- temporary workload. This is incredibly useless; you should
    -- definitely override this default.
    --
    -- Since 0.1.1
    , reaperDelay :: {-# UNPACK #-} !Int
    -- ^ Number of microseconds to delay between calls of 'reaperAction'.
    --
    -- Default: 30 seconds.
    --
    -- Since 0.1.1
    , reaperCons :: item -> workload -> workload
    -- ^ Add an item onto a workload.
    --
    -- Default: list consing.
    --
    -- Since 0.1.1
    , reaperNull :: workload -> Bool
    -- ^ Check if a workload is empty, in which case the worker thread
    -- will shut down.
    --
    -- Default: 'null'.
    --
    -- Since 0.1.1
    , reaperEmpty :: workload
    -- ^ An empty workload.
    --
    -- Default: empty list.
    --
    -- Since 0.1.1
    }

-- | Default @ReaperSettings@ value, biased towards having a list of work
-- items.
--
-- Since 0.1.1
defaultReaperSettings :: ReaperSettings [item] item
defaultReaperSettings = ReaperSettings
    { reaperAction = \wl -> return (wl ++)
    , reaperDelay = 30000000
    , reaperCons = (:)
    , reaperNull = null
    , reaperEmpty = []
    }

-- | A data structure to hold reaper APIs.
data Reaper workload item = Reaper {
    -- | Adding an item to the workload
    reaperAdd  :: item -> IO ()
    -- | Reading workload.
  , reaperRead :: IO workload
    -- | Stopping the reaper thread if exists.
    --   The current workload is returned.
  , reaperStop :: IO workload
  }

-- | State of reaper.
data State workload = NoReaper           -- ^ No reaper thread
                    | Workload workload  -- ^ The current jobs

-- | Create a reaper addition function. This funciton can be used to add
-- new items to the workload. Spawning of reaper threads will be handled
-- for you automatically.
--
-- Since 0.1.1
mkReaper :: ReaperSettings workload item -> IO (Reaper workload item)
mkReaper settings@ReaperSettings{..} = do
    stateRef <- newIORef NoReaper
    return Reaper {
        reaperAdd  = update settings stateRef
      , reaperRead = readRef stateRef
      , reaperStop = stop stateRef
      }
  where
    readRef stateRef = do
        mx <- readIORef stateRef
        case mx of
            NoReaper    -> return reaperEmpty
            Workload wl -> return wl
    stop stateRef = atomicModifyIORef' stateRef $ \mx ->
        case mx of
            NoReaper   -> (NoReaper, reaperEmpty)
            Workload x -> (Workload reaperEmpty, x)

update :: ReaperSettings workload item -> IORef (State workload) -> item
       -> IO ()
update settings@ReaperSettings{..} stateRef item =
    mask_ $ join $ atomicModifyIORef' stateRef cons
  where
    cons NoReaper      = (Workload $ reaperCons item reaperEmpty
                         ,spawn settings stateRef)
    cons (Workload wl) = (Workload $ reaperCons item wl
                         ,return ())

spawn :: ReaperSettings workload item -> IORef (State workload) -> IO ()
spawn settings stateRef = void . forkIO $ reaper settings stateRef

reaper :: ReaperSettings workload item -> IORef (State workload) -> IO ()
reaper settings@ReaperSettings{..} stateRef = do
    threadDelay reaperDelay
    -- Getting the current jobs. Push an empty job to the reference.
    wl <- atomicModifyIORef' stateRef swapWithEmpty
    -- Do the jobs. A function to merge the left jobs and
    -- new jobs is returned.
    merge <- reaperAction wl
    -- Merging the left jobs and new jobs.
    -- If there is no jobs, this thread finishes.
    join $ atomicModifyIORef' stateRef (check merge)
  where
    swapWithEmpty NoReaper      = error "Control.Reaper.reaper: unexpected NoReaper (1)"
    swapWithEmpty (Workload wl) = (Workload reaperEmpty, wl)

    check _ NoReaper   = error "Control.Reaper.reaper: unexpected NoReaper (2)"
    check merge (Workload wl)
      -- If there is no job, reaper is terminated.
      | reaperNull wl' = (NoReaper,  return ())
      -- If there are jobs, carry them out.
      | otherwise      = (Workload wl', reaper settings stateRef)
      where
        wl' = merge wl

-- | A helper function for creating 'reaperAction' functions. You would
-- provide this function with a function to process a single work item and
-- return either a new work item, or @Nothing@ if the work item is
-- expired.
--
-- Since 0.1.1
mkListAction :: (item -> IO (Maybe item'))
             -> [item]
             -> IO ([item'] -> [item'])
mkListAction f =
    go id
  where
    go front [] = return front
    go front (x:xs) = do
        my <- f x
        let front' =
                case my of
                    Nothing -> front
                    Just y  -> front . (y:)
        go front' xs
