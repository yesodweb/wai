{-# LANGUAGE RecordWildCards    #-}

-- | This module provides the ability to create reapers: dedicated cleanup
-- threads. These threads will automatically spawn and die based on the
-- presence of a workload to process on.
module Control.Reaper (
      -- * Type
      ReaperSettings
    , defaultReaperSettings
      -- * Accessors
    , reaperAction
    , reaperDelay
    , reaperCons
    , reaperNull
    , reaperEmpty
      -- * Creation
    , reaper
      -- * Helper
    , mkListAction
    ) where

import Control.AutoUpdate.Util (atomicModifyIORef')
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (mask_)
import Control.Monad (join, void)
import Data.IORef (IORef, newIORef)

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

-- | Create a reaper addition function. This funciton can be used to add
-- new items to the workload. Spawning of reaper threads will be handled
-- for you automatically.
--
-- Since 0.1.1
reaper :: ReaperSettings workload item
       -> IO (item -> IO (), IORef (Maybe workload))
reaper settings = do
    stateRef <- newIORef Nothing
    return (update settings stateRef, stateRef)

update :: ReaperSettings workload item -> IORef (Maybe workload) -> item
       -> IO ()
update settings@ReaperSettings{..} stateRef item =
    mask_ $ join $ atomicModifyIORef' stateRef cons
  where
    cons Nothing   = (Just $ reaperCons item reaperEmpty, spawn settings stateRef)
    cons (Just wl) = (Just $ reaperCons item wl, return ())

spawn :: ReaperSettings workload item -> IORef (Maybe workload) -> IO ()
spawn settings stateRef = void . forkIO $ loop settings stateRef

loop :: ReaperSettings workload item -> IORef (Maybe workload) -> IO ()
loop settings@ReaperSettings{..} stateRef = do
    threadDelay reaperDelay
    wl1 <- atomicModifyIORef' stateRef get
    wl2 <- reaperAction wl1
    join $ atomicModifyIORef' stateRef (check wl2)
  where
    get Nothing   = error "Control.Reaper.loop: unexpected Nothing (1)"
    get (Just wl) = (Just reaperEmpty, wl)

    check _   Nothing  = error "Control.Reaper.loop: unexpected Nothing (2)"
    check wl2 (Just wl3)
      | reaperNull wl4 = (Nothing, return ())
      | otherwise      = (Just wl4, loop settings stateRef)
      where
        wl4 = wl2 wl3

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
