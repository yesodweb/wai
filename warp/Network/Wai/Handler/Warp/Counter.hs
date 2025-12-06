{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Counter (
    Counter,
    newCounter,
    waitForZero,
    increase,
    decrease,
    waitForDecreased,
    getCount,
) where

import Control.Concurrent.STM

import Network.Wai.Handler.Warp.Imports

newtype Counter = Counter (TVar Int)

newCounter :: IO Counter
newCounter = Counter <$> newTVarIO 0

waitForZero :: Counter -> IO ()
waitForZero (Counter var) = atomically $ do
    x <- readTVar var
    when (x > 0) retry

waitForDecreased :: Counter -> IO ()
waitForDecreased (Counter var) = do
    n0 <- atomically $ readTVar var
    atomically $ do
        n <- readTVar var
        check (n < n0)

increase :: Counter -> IO ()
increase (Counter var) = atomically $ modifyTVar' var $ \x -> x + 1

decrease :: Counter -> IO ()
decrease (Counter var) = atomically $ modifyTVar' var $ \x -> x - 1

-- | Get the current count of open connections.
--
-- Since 3.4.11
getCount :: Counter -> IO Int
getCount (Counter var) = readTVarIO var
