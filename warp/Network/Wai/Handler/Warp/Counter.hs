{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Counter (
    Counter,
    newCounter,
    waitForZero,
    increase,
    decrease,
    waitForDecreased,
) where

import Control.Concurrent.STM

import Network.Wai.Handler.Warp.Imports

newtype Counter = Counter (TVar Int)

newCounter :: IO Counter
newCounter = Counter <$> newTVarIO 0

waitForZero :: Counter -> IO ()
waitForZero (Counter ref) = atomically $ do
    x <- readTVar ref
    when (x > 0) retry

waitForDecreased :: Counter -> IO ()
waitForDecreased (Counter ref) = do
    n0 <- atomically $ readTVar ref
    atomically $ do
        n <- readTVar ref
        check (n < n0)

increase :: Counter -> IO ()
increase (Counter ref) = atomically $ modifyTVar' ref $ \x -> x + 1

decrease :: Counter -> IO ()
decrease (Counter ref) = atomically $ modifyTVar' ref $ \x -> x - 1
