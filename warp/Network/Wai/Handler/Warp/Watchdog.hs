{-# LANGUAGE CPP #-}

-- | One timeout supervisor per connection, for HTTP\/1.1 and HTTP\/2 alike.
--
-- The connection records what it is doing in 'TVar's and a separate thread,
-- the watchdog, decides whether it has been doing it for too long. When it
-- has, the watchdog does not throw anything at the connection: it writes
-- 'True' to a 'TVar', and the receiving function, which waits for the socket
-- with STM, composes that 'TVar' with the socket readiness and gives up on
-- its own, at a known point, by throwing 'TimeoutThread' from 'connRecv'.
--
-- Killing the connection thread with an asynchronous exception remains only
-- as a fallback, for a connection that did not notice in time: one blocked
-- in a send, or one whose 'connRecv' cannot wait with STM (Windows, or a
-- custom 'Network.Wai.Handler.Warp.Internal.Connection').
--
-- What counts as "too long" is decided from the state alone:
--
-- 1. A write in progress must make progress ('txTick').
--
-- 2. Otherwise, when Warp waits for the peer on behalf of an application
--    (a request body), the peer must make progress ('rxTick').
--
-- 3. Otherwise, while an application is running there is no limit:
--    how long a handler takes is the application's business.
--
-- 4. Otherwise the connection is idle and the peer must make progress.
--
-- HTTP\/2 needs no special treatment: several streams just mean several
-- applications and several readers at the same time.
module Network.Wai.Handler.Warp.Watchdog (
    Watchdog,
    newWatchdog,

    -- * Recording activity
    rxTick,
    waitingForPeer,
    sending,
    txTick,

    -- * Observing a timeout
    timedOutSTM,
    throwIfTimedOut,

    -- * Supervision
    withWatchdog,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (ThreadId, forkIO, myThreadId, throwTo)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (void)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc.Sync (labelThread)
import System.TimeManager (TimeoutThread (..))

#if !defined(mingw32_HOST_OS)
import qualified GHC.Event as EV
#endif

----------------------------------------------------------------

data Activity = Activity
    { actRx :: !Int
    -- ^ Reads that count as progress of the peer.
    , actTx :: !Int
    -- ^ Completed writes.
    , actSending :: !Int
    -- ^ Writes in progress.
    , actWaiting :: !Int
    -- ^ Reads in progress on behalf of applications.
    }
    deriving (Eq)

-- | The supervised state of one connection.
data Watchdog = Watchdog
    { wdActivity :: TVar Activity
    , wdTimedOut :: TVar Bool
    }

-- | Creating the state. No thread is started until 'withWatchdog'.
newWatchdog :: IO Watchdog
newWatchdog = Watchdog <$> newTVarIO (Activity 0 0 0 0) <*> newTVarIO False

modifyActivity :: Watchdog -> (Activity -> Activity) -> IO ()
modifyActivity wd f = atomically $ modifyTVar' (wdActivity wd) f

-- | The peer made progress.
rxTick :: Watchdog -> IO ()
rxTick wd = modifyActivity wd $ \a -> a{actRx = actRx a + 1}

-- | A write completed or made progress.
txTick :: Watchdog -> IO ()
txTick wd = modifyActivity wd $ \a -> a{actTx = actTx a + 1}

-- | Running a read which waits for the peer on behalf of an application,
--   such as reading a request body.
waitingForPeer :: Watchdog -> IO a -> IO a
waitingForPeer wd =
    E.bracket_
        (modifyActivity wd $ \a -> a{actWaiting = actWaiting a + 1})
        (modifyActivity wd $ \a -> a{actWaiting = actWaiting a - 1})

-- | Running a write. 'txTick' is called when it completes.
sending :: Watchdog -> IO a -> IO a
sending wd act = do
    r <-
        E.bracket_
            (modifyActivity wd $ \a -> a{actSending = actSending a + 1})
            (modifyActivity wd $ \a -> a{actSending = actSending a - 1})
            act
    txTick wd
    return r

----------------------------------------------------------------

-- | Succeeding once the watchdog has decided this connection timed out.
--   Retrying otherwise. To be composed with waiting for the socket.
timedOutSTM :: Watchdog -> STM ()
timedOutSTM wd = readTVar (wdTimedOut wd) >>= check

-- | Throwing 'TimeoutThread' if the connection timed out.
throwIfTimedOut :: Watchdog -> IO ()
throwIfTimedOut wd = do
    timedOut <- readTVarIO $ wdTimedOut wd
    if timedOut then E.throwIO TimeoutThread else return ()

----------------------------------------------------------------

-- | What the connection must do to stay alive.
data Rule
    = Writing !Int
    | ReadingForApp !Int
    | RunningApp
    | Idle !Int
    deriving (Eq)

-- | Two 'Rule's differ when the connection made progress or moved on.
rule :: Activity -> Int -> Rule
rule a apps
    | actSending a > 0 = Writing $ actTx a
    | actWaiting a > 0 = ReadingForApp $ actRx a
    | apps > 0 = RunningApp
    | otherwise = Idle $ actRx a

-- | How long the connection may stay under a rule, in nanoseconds.
budget :: Int -> Rule -> Maybe Int
budget _ RunningApp = Nothing
budget us _ = Just $ us * 1000

data Event = Done | Moved | Expired

-- | Supervising an action.
--
--   The 'TVar' is the number of applications running on this
--   connection. When the timeout is zero or less, no watchdog runs.
--   'TimeoutThread', whether thrown by 'connRecv' or by the fallback, does
--   not escape.
withWatchdog :: Int -> Watchdog -> TVar Int -> IO () -> IO ()
withWatchdog us wd apps action
    | us <= 0 = action
    | otherwise = E.handle ignore $ do
        tid <- myThreadId
        done <- newTVarIO False
        finished <- newTVarIO False
        void $ forkIO $ do
            myThreadId >>= \me -> labelThread me "Warp watchdog"
            watchdog us wd apps done tid
                `E.finally` atomically (writeTVar finished True)
        action `E.finally` do
            atomically $ writeTVar done True
            -- The fallback may be on its way; it is ignored above.
            atomically $ readTVar finished >>= check
  where
    ignore TimeoutThread = return ()

watchdog :: Int -> Watchdog -> TVar Int -> TVar Bool -> ThreadId -> IO ()
watchdog us wd apps done tid = loop Nothing
  where
    -- The watchdog is woken by any change of the connection, and then
    -- sleeps for this interval. However busy the connection is, the
    -- watchdog wakes up once per interval at most, and ticks cost a
    -- write to a 'TVar' only. It is also how long the connection is
    -- given to notice a timeout by itself.
    interval = max 1 $ min 1000000 $ us `div` 4

    snapshot = (,) <$> readTVar (wdActivity wd) <*> readTVar apps

    -- The deadline is kept as long as the rule stays the same, that
    -- is, as long as the connection makes no progress which counts.
    loop prev = do
        snap0@(act, n) <- atomically snapshot
        now <- getNow
        let r = rule act n
            deadline = case prev of
                Just (r', d) | r' == r -> d
                _ -> (now +) <$> budget us r
            remaining = (\d -> max 1 $ (d - now) `div` 1000) <$> deadline
        ev <- waitFor remaining $ (/= snap0) <$> snapshot
        case ev of
            Done -> return ()
            Moved -> do
                ev' <- waitFor (Just interval) (return False)
                case ev' of
                    Done -> return ()
                    _ -> loop $ Just (r, deadline)
            Expired -> do
                atomically $ writeTVar (wdTimedOut wd) True
                ev' <- waitFor (Just interval) (return False)
                case ev' of
                    Done -> return ()
                    -- The connection is blocked where it cannot see the
                    -- 'TVar'. The last resort is the old way.
                    _ -> throwTo tid TimeoutThread

    -- If the connection moved and the timer expired at the same time,
    -- it moved: a connection which turns out to be alive is not killed.
    waitFor mt moved = withTimer mt $ \expired -> atomically $
        (readTVar done >>= check >> return Done)
            <|> (moved >>= check >> return Moved)
            <|> (expired >> return Expired)

-- | Monotonic time in nanoseconds.
getNow :: IO Int
getNow = fromIntegral <$> getMonotonicTimeNSec

----------------------------------------------------------------

-- | Running an action with an STM action which succeeds once the
--   time passed. The timer is cancelled when the action finishes.
withTimer :: Maybe Int -> (STM () -> IO a) -> IO a
withTimer Nothing act = act retry
#if defined(mingw32_HOST_OS)
withTimer (Just t) act = do
    var <- registerDelay t
    act (readTVar var >>= check)
#else
withTimer (Just t) act = do
    var <- newTVarIO False
    mgr <- EV.getSystemTimerManager
    E.bracket
        (EV.registerTimeout mgr t $ atomically $ writeTVar var True)
        (EV.unregisterTimeout mgr)
        $ \_ -> act (readTVar var >>= check)
#endif
