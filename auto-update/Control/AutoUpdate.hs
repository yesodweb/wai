{-# LANGUAGE CPP #-}

-- | In a multithreaded environment, sharing results of actions can dramatically improve performance.
-- For example, web servers need to return the current time with each HTTP response.
-- For a high-volume server, it's much faster for a dedicated thread to run every
-- second, and write the current time to a shared 'IORef', than it is for each
-- request to make its own call to 'getCurrentTime'.
--
-- But for a low-volume server, whose request frequency is less than once per
-- second, that approach will result in /more/ calls to 'getCurrentTime' than
-- necessary, and worse, kills idle GC.
--
-- This library solves that problem by allowing you to define actions which will
-- either be performed by a dedicated thread, or, in times of low volume, will
-- be executed by the calling thread.
--
-- Example usage:
--
-- @
-- import "Data.Time"
-- import "Control.AutoUpdate"
--
-- getTime <- 'mkAutoUpdate' 'defaultUpdateSettings'
--              { 'updateAction' = 'Data.Time.Clock.getCurrentTime'
--              , 'updateFreq' = 1000000 -- The default frequency, once per second
--              }
-- currentTime <- getTime
-- @
--
-- For more examples, <http://www.yesodweb.com/blog/2014/08/announcing-auto-update see the blog post introducing this library>.
module Control.AutoUpdate (
    -- * Type
    UpdateSettings,
    defaultUpdateSettings,

    -- * Accessors
    updateAction,
    updateFreq,
    updateSpawnThreshold,
    updateThreadName,

    -- * Creation
    mkAutoUpdate,
    mkAutoUpdateWithModify,
)
where

import Control.AutoUpdate.Types
#ifdef mingw32_HOST_OS
import Control.AutoUpdate.Thread
#else
import qualified Control.AutoUpdate.Event as Event
import qualified Control.AutoUpdate.Thread as Thread

import GHC.Event

mkAutoUpdate :: UpdateSettings a -> IO (IO a)
mkAutoUpdate settings = do
    mmgr <- getSystemEventManager
    case mmgr of
      Nothing -> Thread.mkAutoUpdate settings
      Just _m -> Event.mkAutoUpdate settings

mkAutoUpdateWithModify :: UpdateSettings a -> (a -> IO a) -> IO (IO a)
mkAutoUpdateWithModify settings f = do
    mmgr <- getSystemEventManager
    case mmgr of
      Nothing -> Thread.mkAutoUpdateWithModify settings f
      Just _m -> Event.mkAutoUpdateWithModify settings f
#endif
