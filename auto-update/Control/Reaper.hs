-- | This module provides the ability to create reapers: dedicated cleanup
-- threads. These threads will automatically spawn and die based on the
-- presence of a workload to process on.
module Control.Reaper
    ( ReaperSettings
    , reaper
    , defaultReaperSettings
    , reaperAction
    , reaperDelay
    , reaperCons
    , reaperNull
    , mkListAction
    ) where

import Prelude hiding (null)
import Data.Monoid (Monoid, mempty)
import Control.Monad (join, void)
import Data.Function (fix)
import Control.AutoUpdate.Util (atomicModifyIORef')
import Control.Concurrent (forkIO, threadDelay)
import Data.IORef (newIORef)
import Control.Exception (mask_)
import qualified Prelude

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
    -- Default: 'Prelude.null'.
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
    , reaperNull = Prelude.null
    }

-- | Create a reaper addition function. This funciton can be used to add
-- new items to the workload. Spawning of reaper threads will be handled
-- for you automatically.
--
-- Since 0.1.1
reaper :: Monoid workload
       => ReaperSettings workload item
       -> IO (item -> IO ())
reaper (ReaperSettings action delay cons null) = do
    stateRef <- newIORef Nothing
    return (update stateRef)
  where
    update stateRef item = mask_ $ join $ atomicModifyIORef' stateRef $ \ms ->
        case ms of
            Nothing -> (Just $ cons item mempty, spawn stateRef)
            Just wl -> (Just $ cons item wl, return ())

    spawn stateRef = void $ forkIO $ fix $ \loop -> do
        threadDelay delay
        wl1 <- atomicModifyIORef' stateRef $ \ms ->
            case ms of
                Nothing -> error "Control.Reaper.reaper.spawn: unexpected Nothing (1)"
                Just wl -> (Just mempty, wl)
        wl2 <- action wl1
        join $ atomicModifyIORef' stateRef $ \ms ->
            case ms of
                Nothing -> error "Control.Reaper.reaper.spawn: unexpected Nothing (2)"
                Just wl3 ->
                    let wl4 = wl2 wl3
                     in if null wl4
                            then (Nothing, return ())
                            else (Just wl4, loop)

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
    loop id
  where
    loop front [] = return front
    loop front (x:xs) = do
        my <- f x
        let front' =
                case my of
                    Nothing -> front
                    Just y -> front . (y:)
        loop front' xs
