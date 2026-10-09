module Network.Wai.Handler.Warp.Counter (
    Counter,
    newCounter,
    HasDecreased (..),
    waitForZero,
    reserve,
    serving,
    waitForDecreased,
    getCount,
    getCountSTM,
    threadManager,
) where

import Control.Concurrent.STM
import System.ThreadManager (
    Reservation,
    ThreadManager,
    countManaged,
    newThreadManager,
    reserveManaged,
    takeReservation,
    waitUntilAllGone,
 )
import qualified System.TimeManager as T


-- | The connections of a server: how many there are, and which threads
--   are serving them.
--
--   Both come from the 'ThreadManager', which counts a connection from
--   when the accept loop keeps a place for it -- before the thread that
--   will serve it has been scheduled, which is too late for a graceful
--   shutdown to notice it.
newtype Counter = Counter ThreadManager

-- | No timeouts are run through this manager, so it is given a dummy
--   time manager rather than one of its own.
newCounter :: IO Counter
newCounter = Counter <$> newThreadManager T.defaultManager

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

-- | Keeping a place for a connection, in the accept loop, before the
--   thread which will serve it is forked.
reserve :: Counter -> IO Reservation
reserve (Counter tm) = reserveManaged tm

-- | Serving a connection on this thread, taking the place kept for it.
serving :: Counter -> Reservation -> IO () -> IO ()
serving (Counter tm) = takeReservation tm

-- | Get the current count of open connections.
--
-- @since 3.4.11
getCount :: Counter -> IO Int
getCount (Counter tm) = atomically $ countManaged tm

-- | Get the current count in an 'STM' transaction.
--
-- @since 3.4.13
getCountSTM :: Counter -> STM Int
getCountSTM (Counter tm) = countManaged tm
