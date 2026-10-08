{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Counter (
    Counter,
    newCounter,
    HasDecreased (..),
    waitForZero,
    increase,
    decrease,
    waitForDecreased,
    getCount,
    getCountSTM,
    serving,
    killAll,
) where

import Control.Concurrent (ThreadId, myThreadId, throwTo)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Data.Set (Set)
import qualified Data.Set as Set

import Network.Wai.Handler.Warp.Imports

-- | How many connections are being served, and on which threads.
--
-- The count is kept apart from the threads because the two are not
-- recorded at the same moment: a connection is counted by the accept loop,
-- which does not wait for the thread it forks to be scheduled, while a
-- thread can only record itself.
data Counter = Counter (TVar Int) (TVar (Set ThreadId))

newCounter :: IO Counter
newCounter = Counter <$> newTVarIO 0 <*> newTVarIO Set.empty

waitForZero :: Counter -> IO ()
waitForZero (Counter var _) = atomically $ do
    x <- readTVar var
    when (x > 0) retry

data HasDecreased = HasDecreased | NoConnections
    deriving (Eq, Show)

waitForDecreased :: Counter -> IO HasDecreased
waitForDecreased (Counter var _) = do
    n0 <- atomically $ readTVar var
    if n0 <= 0
        then pure NoConnections
        else atomically $ do
            n <- readTVar var
            check (n < n0)
            pure HasDecreased

increase :: Counter -> IO ()
increase (Counter var _) = atomically $ modifyTVar' var $ \x -> x + 1

decrease :: Counter -> IO ()
decrease (Counter var _) = atomically $ modifyTVar' var $ \x -> x - 1

-- | Get the current count of open connections.
--
-- @since 3.4.11
getCount :: Counter -> IO Int
getCount (Counter var _) = readTVarIO var

-- | Get the current count in an 'STM' transaction.
--
-- @since 3.4.13
getCountSTM :: Counter -> STM Int
getCountSTM (Counter tvar _) = readTVar tvar

-- | Serving a connection on this thread, so that 'killAll' can reach it.
serving :: Counter -> IO a -> IO a
serving (Counter _ var) action = do
    tid <- myThreadId
    E.bracket_
        (atomically $ modifyTVar' var $ Set.insert tid)
        (atomically $ modifyTVar' var $ Set.delete tid)
        action

-- | Throwing 'E.ThreadKilled' to every connection still being served.
--
-- Warp forks a thread per connection and nothing else owns those threads,
-- so leaving the accept loop without this leaves them running: they are
-- not children of the thread that called 'Network.Wai.Handler.Warp.run'
-- in any sense the runtime knows about.
killAll :: Counter -> IO ()
killAll (Counter _ var) = do
    tids <- atomically $ do
        tids <- readTVar var
        writeTVar var Set.empty
        return tids
    mapM_ (`throwTo` E.ThreadKilled) $ Set.toList tids
