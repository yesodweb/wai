{-# LANGUAGE NumericUnderscores #-}

-- | A timer for Windows which does not depend on the I\/O manager.
--
--   @GHC.Event.Windows@ is WinIO's manager, so a timeout registered with
--   it is never run when the program uses the old Windows I\/O manager,
--   which is still the default.  This does the same job with one thread
--   and 'threadDelay' per timeout, which both managers drive.
module System.TimeManager.Windows (
    Manager,
    TimeoutKey,
    getSystemManager,
    registerTimeout,
    unregisterTimeout,
    updateTimeout,
) where

import Control.Concurrent (ThreadId, forkIO, killThread, threadDelay)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

-- | There is nothing to keep: each timeout is its own thread.
data Manager = Manager

getSystemManager :: IO Manager
getSystemManager = pure Manager

-- | A registered timeout.  The deadline is read by the thread rather
--   than captured, so that 'updateTimeout' does not have to replace it.
data TimeoutKey = TimeoutKey (IORef Word64) ThreadId

instance Eq TimeoutKey where
    TimeoutKey _ t1 == TimeoutKey _ t2 = t1 == t2

-- | Run the action once the given number of microseconds has passed.
registerTimeout :: Manager -> Int -> IO () -> IO TimeoutKey
registerTimeout _ us action = do
    ref <- newIORef =<< deadlineIn us
    tid <- forkIO $ wait ref
    pure $ TimeoutKey ref tid
  where
    wait ref = do
        deadline <- readIORef ref
        now <- getMonotonicTimeNSec
        if now >= deadline
            then action
            else do
                -- Round up, so that a deadline under a microsecond away
                -- is still waited for rather than busy looped on.
                threadDelay $ fromIntegral ((deadline - now + 999) `div` 1_000)
                wait ref

-- | Move the deadline the given number of microseconds into the future.
updateTimeout :: Manager -> TimeoutKey -> Int -> IO ()
updateTimeout _ (TimeoutKey ref _) us = writeIORef ref =<< deadlineIn us

unregisterTimeout :: Manager -> TimeoutKey -> IO ()
unregisterTimeout _ (TimeoutKey _ tid) = killThread tid

deadlineIn :: Int -> IO Word64
deadlineIn us = do
    now <- getMonotonicTimeNSec
    pure $ now + fromIntegral (max 0 us) * 1_000
