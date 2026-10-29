{-# LANGUAGE RankNTypes #-}

module Network.Wai.Handler.Warp.Counter (
    Counter,
    newCounter,
    HasDecreased (..),
    waitForZero,
    forkConnection,
    waitForDecreased,
    getCount,
    getCountSTM,
    threadManager,
) where

import Control.Concurrent.STM
import System.ThreadManager (
    ThreadManager,
    countManaged,
    forkManagedBy,
    newThreadManager,
    waitUntilAllGone,
 )

-- | The connections of a server: how many there are, and which threads
--   are serving them.
--
--   Both come from the 'ThreadManager', which counts a connection from
--   when the accept loop keeps a place for it -- before the thread that
--   will serve it has been scheduled, which is too late for a graceful
--   shutdown to notice it.
newtype Counter = Counter ThreadManager

newCounter :: IO Counter
newCounter = Counter <$> newThreadManager

-- | The manager owning the connections, for supervising them.
threadManager :: Counter -> ThreadManager
threadManager (Counter tm) = tm

waitForZero :: Counter -> IO ()
waitForZero (Counter tm) = waitUntilAllGone tm

data HasDecreased = HasDecreased | NoConnections
    deriving (Eq, Show)

waitForDecreased :: Counter -> IO HasDecreased
waitForDecreased (Counter tm) = do
    n0 <- atomically $ countManaged tm
    if n0 <= 0
        then pure NoConnections
        else atomically $ do
            n <- countManaged tm
            check (n < n0)
            pure HasDecreased

-- | Serving a connection on a thread made by the given fork.  The
--   connection is counted from here rather than from when that thread
--   starts: the accept loop does not wait for it, and a connection which
--   has been accepted must not look to 'waitForZero' like one that is
--   gone.
forkConnection
    :: Counter
    -> (((forall x. IO x -> IO x) -> IO ()) -> IO ())
    -- ^ The fork from the settings
    -> ((forall x. IO x -> IO x) -> IO ())
    -- ^ Serving the connection
    -> IO ()
forkConnection (Counter tm) fork io = forkManagedBy tm fork "Warp just forked" io

-- | Get the current count of open connections.
--
-- @since 3.4.11
getCount :: Counter -> IO Int
getCount = atomically . getCountSTM

-- | Get the current count in an 'STM' transaction.
--
-- @since 3.4.13
getCountSTM :: Counter -> STM Int
getCountSTM (Counter tm) = countManaged tm
