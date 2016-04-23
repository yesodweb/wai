{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE BangPatterns       #-}

-- | This module provides the ability to create reapers: dedicated cleanup
-- threads. These threads will automatically spawn and die based on the
-- presence of a workload to process on. Example uses include:
--
-- * Killing long-running jobs (see example below)
-- * Closing unused connections in a connection pool
-- * Pruning a cache of old items
--
-- For real-world usage, search the <https://github.com/yesodweb/wai WAI family of packages>
-- for imports of "Control.Reaper".
module Control.Reaper (
      -- * Example: A small jobs system
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
    -- workload to the temporary workload. For help with setting up such
    -- an action, see 'mkListAction'.
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
                    | Workload workload  -- ^ The current jobs

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
    cons NoReaper      = let !wl = reaperCons item reaperEmpty
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
-- In this example code, we create a small jobs system.
-- The @main@ function first creates a 'Reaper' to manage our jobs,
-- then starts 10 jobs that each take 1 to 10 seconds to complete.
-- Each Job is added to the Reaper's workload (a list of Jobs).
--
-- Every second, the 'Reaper' will traverse the list of jobs, removing completed ones,
-- and killing ones that have run longer than 5 seconds.
--
-- Once all jobs are completed or killed, the 'Reaper''s own thread will die.
--
-- @
-- {-\# LANGUAGE MultiWayIf #-}
--
-- module Main where
--
-- import "Data.Time"
-- import "System.Random"
-- import "Control.Reaper"
-- import "Control.Concurrent"
-- import "Control.Monad"
-- import "Data.IORef"
--
-- -- In this example code, we add Jobs (the individual items) to our Reaper's workload (a list of Jobs)
-- data Job = Job { jobID :: 'Int'
--                , jobStarted :: 'Data.Time.Clock.UTCTime'
--                , jobThreadId :: 'ThreadId'
--                , jobFinished :: 'IORef' 'Bool'
--                }
--
-- main :: IO ()
-- main = do
--   reaper <- 'mkReaper' 'defaultReaperSettings'
--             { 'reaperAction' = 'mkListAction' maybeReapJob
--             , 'reaperDelay' = 1000000 -- Check for completed/valid jobs every second.
--             }
--   'forM_' [1..10] $ \\i -> do
--     job <- startJob i
--     ('reaperAdd' reaper) job
--
--   'threadDelay' 1500000 -- Sleep 15 seconds, so that jobs can be completed or killed.
--
-- -- Start a variable-length task that takes between 1 and 10 seconds
-- startJob :: Int -> IO Job
-- startJob aJobID = do
--   startTime <- 'Data.Time.Clock.getCurrentTime'
--   finishedRef <- 'newIORef' 'False'
--   threadID <- 'forkIO' $ do
--     secondsToWait <- 'System.Random.getStdRandom' ('System.Random.randomR' (1,10))
--     'threadDelay' (secondsToWait * 1000000)
--     'Data.IORef.atomicWriteIORef' finishedRef 'True' -- Mark the job as complete.
--   'return' $ Job aJobID startTime threadID finishedRef
--
-- -- Remove completed jobs, and also kill ones that have run longer than 5 seconds.
-- maybeReapJob :: Job -> IO (Maybe Job)
-- maybeReapJob job = do
--   currentTime <- 'Data.Time.Clock.getCurrentTime'
--   let runningTime = currentTime \`diffUTCTime\` (jobStarted job)
--   jobDone <- 'readIORef' (jobFinished job)
--
--   if | jobDone -> do
--         'putStrLn' $ "Completed job #" ++ 'show' (jobID job)
--         'return' 'Nothing'
--      | runningTime > 5.0 -> do
--          'killThread' (jobThreadId job)
--          'putStrLn' $ "Killed job #" ++ 'show' (jobID job)
--          'return' 'Nothing'
--      | otherwise -> 'return' ('Just' job)
-- @