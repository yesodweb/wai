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

data HasDecreased = HasDecreased | NoConnections
    deriving (Eq, Show)

waitForDecreased :: Counter -> IO HasDecreased
waitForDecreased (Counter var) = do
    n0 <- atomically $ readTVar var
    if n0 <= 0
        then pure NoConnections
        else atomically $ do
            n <- readTVar var
            check (n < n0)
            pure HasDecreased

increase :: Counter -> IO ()
increase (Counter var) = atomically $ modifyTVar' var $ \x -> x + 1

decrease :: Counter -> IO ()
decrease (Counter var) = atomically $ modifyTVar' var $ \x -> x - 1

-- | Get the current count of open connections.
--
-- Since 3.4.11
getCount :: Counter -> IO Int
getCount (Counter var) = readTVarIO var

-- | Get the current count in an 'STM' transaction.
--
-- Since 3.4.13
getCountSTM :: Counter -> STM Int
getCountSTM (Counter tvar) = readTVar tvar
