{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE BangPatterns       #-}

-- | This module provides the ability to create reapers: dedicated cleanup
-- threads. These threads will automatically spawn and die based on the
-- presence of a workload to process on. Example uses include:
--
-- * Killing long-running jobs
-- * Closing unused connections in a connection pool
-- * Pruning a cache of old items (see example below)
--
-- For real-world usage, search the <https://github.com/yesodweb/wai WAI family of packages>
-- for imports of "Control.Reaper".
module Control.Reaper (
      -- * Example: Regularly cleaning a cache
      -- $example1

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
import Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import Control.Exception (mask_)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | Settings for creating a reaper. This type has two parameters:
-- @workload@ gives the entire workload, whereas @item@ gives an
-- individual piece of the queue. A common approach is to have @workload@
-- be a list of @item@s. This is encouraged by 'defaultReaperSettings' and
-- 'mkListAction'.
--
-- @since 0.1.1
data ReaperSettings workload item = ReaperSettings
    { reaperAction :: workload -> IO (workload -> workload)
    -- ^ The action to perform on a workload. The result of this is a
    -- \"workload modifying\" function. In the common case of using lists,
    -- the result should be a difference list that prepends the remaining
    -- workload to the temporary workload. The temporary workload here
    -- refers to items added to the workload while the reaper action is
    -- running. For help with setting up such an action, see 'mkListAction'.
    --
    -- Default: do nothing with the workload, and then prepend it to the
    -- temporary workload. This is incredibly useless; you should
    -- definitely override this default.
    --
    -- @since 0.1.1
    , reaperDelay :: {-# UNPACK #-} !Int
    -- ^ Number of microseconds to delay between calls of 'reaperAction'.
    --
    -- Default: 30 seconds.
    --
    -- @since 0.1.1
    , reaperCons :: item -> workload -> workload
    -- ^ Add an item onto a workload.
    --
    -- Default: list consing.
    --
    -- @since 0.1.1
    , reaperNull :: workload -> Bool
    -- ^ Check if a workload is empty, in which case the worker thread
    -- will shut down.
    --
    -- Default: 'null'.
    --
    -- @since 0.1.1
    , reaperEmpty :: workload
    -- ^ An empty workload.
    --
    -- Default: empty list.
    --
    -- @since 0.1.1
    }

-- | Default @ReaperSettings@ value, biased towards having a list of work
-- items.
--
-- @since 0.1.1
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
    -- | Killing the reaper thread immediately if exists.
  , reaperKill :: IO ()
  }

-- | State of reaper.
data State workload = NoReaper           -- ^ No reaper thread
                    | Workload !workload  -- ^ The current jobs

-- | Create a reaper addition function. This function can be used to add
-- new items to the workload. Spawning of reaper threads will be handled
-- for you automatically.
--
-- @since 0.1.1
mkReaper :: ReaperSettings workload item -> IO (Reaper workload item)
mkReaper settings@ReaperSettings{..} = do
    stateRef <- newIORef NoReaper
    tidRef   <- newIORef Nothing
    return Reaper {
        reaperAdd  = add settings stateRef tidRef
      , reaperRead = readRef stateRef
      , reaperStop = stop stateRef
      , reaperKill = kill tidRef
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
    kill tidRef = do
        mtid <- readIORef tidRef
        case mtid of
            Nothing  -> return ()
            Just tid -> killThread tid

add :: ReaperSettings workload item
    -> IORef (State workload) -> IORef (Maybe ThreadId)
    -> item -> IO ()
add settings@ReaperSettings{..} stateRef tidRef item =
    mask_ $ do
      next <- atomicModifyIORef' stateRef cons
      next
  where
    cons NoReaper      = let wl = reaperCons item reaperEmpty
                         in (Workload wl, spawn settings stateRef tidRef)
    cons (Workload wl) = let wl' = reaperCons item wl
                         in (Workload wl', return ())

spawn :: ReaperSettings workload item
      -> IORef (State workload) -> IORef (Maybe ThreadId)
      -> IO ()
spawn settings stateRef tidRef = do
    tid <- forkIO $ reaper settings stateRef tidRef
    writeIORef tidRef $ Just tid

reaper :: ReaperSettings workload item
       -> IORef (State workload) -> IORef (Maybe ThreadId)
       -> IO ()
reaper settings@ReaperSettings{..} stateRef tidRef = do
    threadDelay reaperDelay
    -- Getting the current jobs. Push an empty job to the reference.
    wl <- atomicModifyIORef' stateRef swapWithEmpty
    -- Do the jobs. A function to merge the left jobs and
    -- new jobs is returned.
    !merge <- reaperAction wl
    -- Merging the left jobs and new jobs.
    -- If there is no jobs, this thread finishes.
    next <- atomicModifyIORef' stateRef (check merge)
    next
  where
    swapWithEmpty NoReaper      = error "Control.Reaper.reaper: unexpected NoReaper (1)"
    swapWithEmpty (Workload wl) = (Workload reaperEmpty, wl)

    check _ NoReaper   = error "Control.Reaper.reaper: unexpected NoReaper (2)"
    check merge (Workload wl)
      -- If there is no job, reaper is terminated.
      | reaperNull wl' = (NoReaper, writeIORef tidRef Nothing)
      -- If there are jobs, carry them out.
      | otherwise      = (Workload wl', reaper settings stateRef tidRef)
      where
        wl' = merge wl

-- | A helper function for creating 'reaperAction' functions. You would
-- provide this function with a function to process a single work item and
-- return either a new work item, or @Nothing@ if the work item is
-- expired.
--
-- @since 0.1.1
mkListAction :: (item -> IO (Maybe item'))
             -> [item]
             -> IO ([item'] -> [item'])
mkListAction f =
    go id
  where
    go !front [] = return front
    go !front (x:xs) = do
        my <- f x
        let front' =
                case my of
                    Nothing -> front
                    Just y  -> front . (y:)
        go front' xs

-- $example1
-- In this example code, we use a 'Data.Map.Strict.Map' to cache fibonacci numbers, and a 'Reaper' to prune the cache.
--
-- The @main@ function first creates a 'Reaper', with fields to initialize the
-- cache ('reaperEmpty'), add items to it ('reaperCons'), and prune it ('reaperAction').
-- The reaper will run every two seconds ('reaperDelay'), but will stop running while
-- 'reaperNull' is true.
--
-- @main@ then loops infinitely ('Control.Monad.forever'). Each second it calculates the fibonacci number
-- for a value between 30 and 34, first trying the cache ('reaperRead' and 'Data.Map.Strict.lookup'),
-- then falling back to manually calculating it (@fib@)
-- and updating the cache with the result ('reaperAdd')
--
-- @clean@ simply removes items cached for more than 10 seconds.
-- This function is where you would perform IO-related cleanup,
-- like killing threads or closing connections, if that was the purpose of your reaper.
--
-- @
-- module Main where
--
-- import "Data.Time" (UTCTime, getCurrentTime, diffUTCTime)
-- import "Control.Reaper"
-- import "Control.Concurrent" (threadDelay)
-- import "Data.Map.Strict" (Map)
-- import qualified "Data.Map.Strict" as Map
-- import "Control.Monad" (forever)
-- import "System.Random" (getStdRandom, randomR)
--
-- fib :: 'Int' -> 'Int'
-- fib 0 = 0
-- fib 1 = 1
-- fib n = fib (n-1) + fib (n-2)
--
-- type Cache = 'Data.Map.Strict.Map' 'Int' ('Int', 'Data.Time.Clock.UTCTime')
--
-- main :: IO ()
-- main = do
--   reaper <- 'mkReaper' 'defaultReaperSettings'
--     { 'reaperEmpty' = Map.'Data.Map.Strict.empty'
--     , 'reaperCons' = \\(k, v, time) workload -> Map.'Data.Map.Strict.insert' k (v, time) workload
--     , 'reaperAction' = clean
--     , 'reaperDelay' = 1000000 * 2 -- Clean every 2 seconds
--     , 'reaperNull' = Map.'Data.Map.Strict.null'
--     }
--   forever $ do
--     fibArg <- 'System.Random.getStdRandom' ('System.Random.randomR' (30,34))
--     cache <- 'reaperRead' reaper
--     let cachedResult = Map.'Data.Map.Strict.lookup' fibArg cache
--     case cachedResult of
--       'Just' (fibResult, _createdAt) -> 'putStrLn' $ "Found in cache: `fib " ++ 'show' fibArg ++ "` " ++ 'show' fibResult
--       'Nothing' -> do
--         let fibResult = fib fibArg
--         'putStrLn' $ "Calculating `fib " ++ 'show' fibArg ++ "` " ++ 'show' fibResult
--         time <- 'Data.Time.Clock.getCurrentTime'
--         ('reaperAdd' reaper) (fibArg, fibResult, time)
--     'threadDelay' 1000000 -- 1 second
--
-- -- Remove items > 10 seconds old
-- clean :: Cache -> IO (Cache -> Cache)
-- clean oldMap = do
--   currentTime <- 'Data.Time.Clock.getCurrentTime'
--   let pruned = Map.'Data.Map.Strict.filter' (\\(_, createdAt) -> currentTime \`diffUTCTime\` createdAt < 10.0) oldMap
--   return (\\newData -> Map.'Data.Map.Strict.union' pruned newData)
-- @
