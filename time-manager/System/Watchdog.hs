{-# LANGUAGE CPP #-}

-- | One timeout supervisor per connection.
--
-- A connection records what it is doing in 'TVar's, and a separate
-- thread, the watchdog, decides whether it has been doing it for too
-- long. When it has, the watchdog does not throw anything at the
-- connection: it writes 'True' to a 'TVar'. The threads of the
-- connection compose 'timedOutSTM' with what they wait for in STM (the
-- socket, a queue) and finish by themselves, at a known point.
--
-- Throwing to a thread remains only as the last resort, given to
-- 'withWatchdog', for a connection which has not finished by itself in
-- time: one blocked in a send, for instance.
--
-- What counts as "too long" is decided from the state alone:
--
-- 1. A write in progress must make progress.
--
-- 2. Otherwise, while a read is waited for on behalf of an application,
--    such as a request body, the peer must make progress.
--
-- 3. Otherwise, while an application is running there is no limit: how
--    long a handler takes is the application's business.
--
-- 4. Otherwise the connection is idle and the peer must make progress.
--
-- Progress is what is reported with 'rxTick' (the peer) and 'sending'
-- or 'txTick' (a write). Under rules 1, 2 and 4, progress restarts the
-- timer, and so does moving from one rule to another. The watchdog looks
-- at the connection at most once per a quarter of the timeout (capped at
-- one second), so a timeout may fire up to that much late, never early.
--
-- A watchdog may be shared by several layers: Warp and the http2 library
-- record into the same one, and only the first 'withWatchdog' runs the
-- thread.
module System.Watchdog (
    Watchdog,
    newWatchdog,

    -- * Recording activity
    rxTick,
    txTick,
    sending,
    waitingForPeer,
    runningApp,

    -- * Observing a timeout
    timedOutSTM,
    isTimedOut,

    -- * Supervision
    withWatchdog,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (void, when)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc.Sync (labelThread, myThreadId)

#if !defined(mingw32_HOST_OS)
import qualified GHC.Event as EV
#endif

----------------------------------------------------------------

data Activity = Activity
    { actRx :: Int
    -- ^ Progress of the peer.
    , actTx :: Int
    -- ^ Completed writes.
    , actSending :: Int
    -- ^ Writes in progress.
    , actWaiting :: Int
    -- ^ Reads in progress on behalf of applications.
    , actApps :: Int
    -- ^ Applications running.
    }
    deriving (Eq)

-- | The supervised state of one connection.
data Watchdog = Watchdog
    { wdTimeout :: Int
    , wdActivity :: TVar Activity
    , wdTimedOut :: TVar Bool
    , wdSupervised :: TVar Bool
    }

-- | Creating the state of a connection with a timeout in microseconds.
--   With zero or less, the connection never times out, recording
--   activity costs nothing, and no thread is started.
newWatchdog :: Int -> IO Watchdog
newWatchdog us =
    Watchdog us
        <$> newTVarIO (Activity 0 0 0 0 0)
        <*> newTVarIO False
        <*> newTVarIO False

enabled :: Watchdog -> Bool
enabled wd = wdTimeout wd > 0

modifyActivity :: Watchdog -> (Activity -> Activity) -> IO ()
modifyActivity wd f =
    when (enabled wd) $ atomically $ modifyTVar' (wdActivity wd) f

-- | The peer made progress.
rxTick :: Watchdog -> IO ()
rxTick wd = modifyActivity wd $ \a -> a{actRx = actRx a + 1}

-- | A write made progress, such as a part of a file sent.
txTick :: Watchdog -> IO ()
txTick wd = modifyActivity wd $ \a -> a{actTx = actTx a + 1}

during :: (Activity -> Activity) -> (Activity -> Activity) -> Watchdog -> IO a -> IO a
during begin end wd act
    | enabled wd = E.bracket_ (modifyActivity wd begin) (modifyActivity wd end) act
    | otherwise = act

-- | Running a write. 'txTick' is called when it completes.
sending :: Watchdog -> IO a -> IO a
sending wd act = do
    r <-
        during
            (\a -> a{actSending = actSending a + 1})
            (\a -> a{actSending = actSending a - 1})
            wd
            act
    txTick wd
    return r

-- | Running a read which waits for the peer on behalf of an
--   application, such as reading a request body.
waitingForPeer :: Watchdog -> IO a -> IO a
waitingForPeer =
    during
        (\a -> a{actWaiting = actWaiting a + 1})
        (\a -> a{actWaiting = actWaiting a - 1})

-- | Running an application.
runningApp :: Watchdog -> IO a -> IO a
runningApp =
    during
        (\a -> a{actApps = actApps a + 1})
        (\a -> a{actApps = actApps a - 1})

----------------------------------------------------------------

-- | Succeeding once the watchdog has decided that the connection timed
--   out. Retrying otherwise.
timedOutSTM :: Watchdog -> STM ()
timedOutSTM wd = readTVar (wdTimedOut wd) >>= check

-- | Whether the watchdog has decided that the connection timed out.
isTimedOut :: Watchdog -> IO Bool
isTimedOut wd = readTVarIO $ wdTimedOut wd

----------------------------------------------------------------

-- | What the connection must do to stay alive. Two 'Rule's differ when
--   the connection made progress which counts, or moved on.
data Rule
    = Writing Int
    | ReadingForApp Int
    | RunningApp
    | Idle Int
    deriving (Eq)

rule :: Activity -> Rule
rule a
    | actSending a > 0 = Writing $ actTx a
    | actWaiting a > 0 = ReadingForApp $ actRx a
    | actApps a > 0 = RunningApp
    | otherwise = Idle $ actRx a

data Event = Done | Moved | Expired

-- | Supervising an action with the watchdog thread.
--
--   If the connection has not finished by itself a while after it timed
--   out, the second argument is run as the last resort, typically
--   throwing an exception to the thread running the action.
--
--   When the watchdog is disabled, or is already supervised by an outer
--   'withWatchdog', this only runs the action.
withWatchdog :: Watchdog -> IO () -> IO a -> IO a
withWatchdog wd lastResort action
    | not (enabled wd) = action
    | otherwise = do
        first <- atomically $ do
            supervised <- readTVar $ wdSupervised wd
            writeTVar (wdSupervised wd) True
            return $ not supervised
        if not first
            then action
            else do
                done <- newTVarIO False
                finished <- newTVarIO False
                void $ forkIO $ do
                    myThreadId >>= \me -> labelThread me "watchdog"
                    watchdog wd lastResort done
                        `E.finally` atomically (writeTVar finished True)
                action `E.finally` do
                    atomically $ writeTVar done True
                    -- The last resort may be on its way.
                    atomically $ readTVar finished >>= check

watchdog :: Watchdog -> IO () -> TVar Bool -> IO ()
watchdog wd lastResort done = loop Nothing
  where
    us = wdTimeout wd

    -- The watchdog is woken by any change of the connection, and then
    -- sleeps for this interval. However busy the connection is, it
    -- wakes up once per interval at most, and recording activity costs
    -- a write to a 'TVar' only. It is also how long the connection is
    -- given to finish by itself after it timed out.
    interval = max 1 $ min 1000000 $ us `div` 4

    -- In nanoseconds. No limit while an application is running.
    budget RunningApp = Nothing
    budget _ = Just $ us * 1000

    snapshot = readTVar $ wdActivity wd

    -- The deadline is kept as long as the rule stays the same, that is,
    -- as long as the connection makes no progress which counts.
    loop prev = do
        act <- atomically snapshot
        now <- getNow
        let r = rule act
            deadline = case prev of
                Just (r', d) | r' == r -> d
                _ -> (now +) <$> budget r
            remaining = (\d -> max 1 $ (d - now) `div` 1000) <$> deadline
        ev <- waitFor remaining $ (/= act) <$> snapshot
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
                    _ -> lastResort

    -- If the connection moved and the timer expired at the same time, it
    -- moved: a connection which turns out to be alive is not given up on.
    waitFor mt moved = withTimer mt $ \expired ->
        atomically $
            (readTVar done >>= check >> return Done)
                <|> (moved >>= check >> return Moved)
                <|> (expired >> return Expired)

-- | Monotonic time in nanoseconds.
getNow :: IO Int
getNow = fromIntegral <$> getMonotonicTimeNSec

----------------------------------------------------------------

-- | Running an action with an STM action which succeeds once the time
--   passed. The timer is cancelled when the action finishes.
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
