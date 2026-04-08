-- Most important is to not export the data constructor from this module
-- and to not expose 'writeShuttingDown' to the end user.
module Network.Wai.Handler.Warp.ShuttingDown (
    ShuttingDown,
    newShuttingDown,
    readShuttingDown,
    readShuttingDownSTM,
    writeShuttingDown,
) where

import Control.Concurrent.STM (
    STM,
    TVar,
    atomically,
    newTVarIO,
    readTVar,
    readTVarIO,
    writeTVar,
 )

newtype ShuttingDown = ShuttingDown (TVar Bool)

newShuttingDown :: IO ShuttingDown
newShuttingDown = ShuttingDown <$> newTVarIO False

readShuttingDown :: ShuttingDown -> IO Bool
readShuttingDown (ShuttingDown var) = readTVarIO var

readShuttingDownSTM :: ShuttingDown -> STM Bool
readShuttingDownSTM (ShuttingDown var) = readTVar var

writeShuttingDown :: ShuttingDown -> Bool -> IO ()
writeShuttingDown (ShuttingDown var) b =
    atomically $ writeTVar var b
