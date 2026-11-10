{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | One timeout supervisor per task.
--
-- A task is anything which runs for a while and should not take too long
-- at what it is doing: serving a network connection, running a job,
-- talking to a device. It is an alternative to "System.TimeManager":
-- rather than tickling a timer, the task records what it is doing in a
-- context, and a separate thread, the watchdog, decides from that alone
-- whether it has been doing it for too long.
--
-- When it has, the watchdog does not throw anything at the task: it
-- writes 'True' to a 'TVar'. The threads of the task compose
-- 'timedOutSTM' with what they wait for in STM (a socket, a queue) and
-- finish by themselves, at a known point. Throwing to a thread remains
-- only as the last resort, given to 'withWatchdog', for a task which has
-- not finished by itself in time: one blocked in a foreign call, say.
--
-- For instance, a worker which may wait for its next job as long as it
-- likes, but must not spend more than five seconds on reading one:
--
-- > data Worker
-- >
-- > instance WatchdogFor Worker where
-- >     data ContextFor Worker = Idle | Reading
-- >     decide _ Idle = return unlimited
-- >     decide _ Reading = return $ setTimeout 5000000
-- >
-- > worker :: TQueue Job -> IO ()
-- > worker q = do
-- >     wd <- newWatchdog Idle
-- >     tid <- myThreadId
-- >     withWatchdog wd (throwTo tid WorkerTimeout) $ do
-- >         update (const Reading) wd
-- >         mjob <- atomically $
-- >             (Just <$> readTQueue q) <|> (Nothing <$ timedOutSTM wd)
-- >         ...
--
-- What the context holds, and what counts as too long, are not decided
-- here. A user gives a type of its own to 'WatchdogFor': 'ContextFor' says
-- what it records, and 'decide' says what the watchdog should do when the
-- record changes. Warp, for instance, records the phase an HTTP\/1.1
-- connection is in, so that it can give reading a request header a
-- shorter leash than writing a response, while the application runs with
-- no limit at all.
--
-- A watchdog belongs to one owner, for as long as that owner has the
-- task. When the task is handed to someone else -- Warp handing a
-- connection to the http2 library, say -- the watchdog is handed over
-- with 'handOver' (or by a 'decide' which answers 'abort'): it stops
-- deciding anything about the task, and the new owner supervises what
-- follows with a watchdog of its own. A task is never watched by two
-- watchdogs at once.
--
-- The watchdog looks at the task at most once per a quarter of the
-- timeout it is waiting out (capped at one second), so a timeout may fire
-- up to that much late, never early.
module System.Watchdog (
    -- * A watchdog, and what it watches
    Watchdog,
    newWatchdog,
    WatchdogFor (..),

    -- * Supervision
    withWatchdog,

    -- * What the watchdog does next
    Action,
    ignore,
    setTimeout,
    unlimited,
    abort,
    timedOut,

    -- * Recording what the task is doing
    update,
    updateSTM,

    -- * Observing a timeout
    timedOutSTM,
    isTimedOut,

    -- * Handing the task on
    handOver,
) where

import Control.Applicative ((<|>))
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import qualified Control.Exception as E
import Control.Monad (void)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc.Sync (labelThread, myThreadId)

#if !defined(mingw32_HOST_OS)
import qualified GHC.Event as EV
#endif

----------------------------------------------------------------

-- | A kind of task, with what is recorded about it and how that is
--   judged: @a@ names the kind, not a task. Warp's HTTP\/1.1 connections
--   and the http2 library's are two.
class WatchdogFor a where
    -- | What a task of this kind is doing, as it records it: which phase it is in, how much has gone by, and whatever
    --   else 'decide' needs, the timeout among it.
    data ContextFor a

    -- | @decide old new@, where @new@ is what the context has become
    --   since the watchdog last looked at it and @old@ is what it was
    --   then. Equal contexts mean the task ended up where it
    --   started. 'Nothing' is the watchdog starting, before the
    --   task has done anything: answering 'ignore' there leaves it
    --   with no timeout until something changes.
    --
    --   The watchdog collapses a burst of updates into one call, so this
    --   is asked about where the task ended up, not about every
    --   step on the way.
    --
    --   It runs on the watchdog's thread and must return at once: while
    --   it blocks nothing times out, and 'withWatchdog' cannot return.
    --   An exception from it ends the supervision of the task.
    decide :: Maybe (ContextFor a) -> ContextFor a -> IO Action

-- | What the watchdog does with the timeout it is running.
data Action
    = Ignore
    | -- | In microseconds.
      SetTimeout !Word64
    | Unlimited
    | Abort
    | TimedOut

-- | Leave the timeout alone. The task has not done anything that
--   counts, so its time keeps ticking down.
ignore :: Action
ignore = Ignore

-- | Start the timeout again with this many microseconds. Zero times out
--   at once.
setTimeout :: Word64 -> Action
setTimeout = SetTimeout

-- | No limit. The task may take as long as it likes until the
--   context changes again: an application is running, say.
unlimited :: Action
unlimited = Unlimited

-- | Stop watching this task, for good. The same as 'handOver'.
abort :: Action
abort = Abort

-- | Time out now. The same as @'setTimeout' 0@.
timedOut :: Action
timedOut = TimedOut

----------------------------------------------------------------

-- | The context, with a count of the updates made to it. The watchdog
--   waits for the count to move rather than compare two contexts, so a
--   context needs no 'Eq'.
data Recorded a = Recorded !Int !(ContextFor a)

-- | The supervised state of one task.
data Watchdog a = Watchdog
    { wdRecorded :: TVar (Recorded a)
    , wdTimedOut :: TVar Bool
    , wdHandedOver :: TVar Bool
    }

-- | Creating the state of a task, with what it is doing now.
newWatchdog :: ContextFor a -> IO (Watchdog a)
newWatchdog ctx = do
    wdRecorded <- newTVarIO $ Recorded 0 ctx
    wdTimedOut <- newTVarIO False
    wdHandedOver <- newTVarIO False
    return Watchdog{..}

-- | Recording what the task is now doing. This is how a
--   task tells its watchdog about any change, and each change is
--   what 'decide' is asked about.
--
--   For example, in HTTP\/1 connection handling, 'update' would be
--   called when the headers have been received, to signify that the
--   application starts running server code, which would probably result
--   in an 'unlimited' action, until 'update' is called again when the
--   request body starts being read.
update :: (ContextFor a -> ContextFor a) -> Watchdog a -> IO ()
update f = atomically . updateSTM f

-- | 'update' as part of a transaction of the caller's own.
updateSTM :: (ContextFor a -> ContextFor a) -> Watchdog a -> STM ()
updateSTM f wd = modifyTVar' (wdRecorded wd) $ \(Recorded n ctx) ->
    Recorded (n + 1) (f ctx)

----------------------------------------------------------------

-- | Succeeding once the watchdog has decided that the task timed
--   out. Retrying otherwise, and for good once the task has been
--   handed over.
timedOutSTM :: Watchdog a -> STM ()
timedOutSTM wd = decided wd >>= check

-- | Whether the watchdog has decided that the task timed out.
--
--   Will always return 'False' if the watchdog has been stopped with
--   'abort', or 'handOver' has been used.
isTimedOut :: Watchdog a -> IO Bool
isTimedOut wd = atomically $ decided wd

decided :: Watchdog a -> STM Bool
decided wd = do
    out <- readTVar $ wdTimedOut wd
    if out
        then not <$> readTVar (wdHandedOver wd)
        else pure False

----------------------------------------------------------------

-- | Handing the task to another owner, which supervises what
--   follows with a watchdog of its own. Equivalent to 'abort'ing the
--   watchdog.
--
--   This watchdog stops: its thread ends, 'timedOutSTM' never succeeds
--   again, and the last resort given to 'withWatchdog' does not run --
--   not even if it had decided a moment before, because that decision
--   was about a task this watchdog no longer watches. 'update'ing
--   afterwards does nothing.
--
--   There is no way back. Once a task is handed over, it's done.
handOver :: Watchdog a -> IO ()
handOver wd = atomically $ writeTVar (wdHandedOver wd) True

----------------------------------------------------------------

data Event = Stop | Changed | Expired

-- | Supervising a body with the watchdog thread, once for each
--   watchdog.
--
--   If the task has not finished by itself a while after it timed
--   out, the second argument is run as the last resort, typically
--   throwing an exception to the thread running the body.
withWatchdog :: WatchdogFor a => Watchdog a -> IO () -> IO b -> IO b
withWatchdog wd lastResort body = do
    done <- newTVarIO False
    finished <- newTVarIO False
    void $ forkIO $ do
        myThreadId >>= \me -> labelThread me "watchdog"
        watchdog wd lastResort done
            `E.finally` atomically (writeTVar finished True)
    body `E.finally` do
        atomically $ writeTVar done True
        -- The last resort may be on its way.
        atomically $ readTVar finished >>= check

watchdog :: WatchdogFor a => Watchdog a -> IO () -> TVar Bool -> IO ()
watchdog wd lastResort done = do
    Recorded n curr <- readTVarIO $ wdRecorded wd
    action <- decide Nothing curr
    loop n curr action Nothing Nothing
  where
    -- Applying what 'decide' answered, then waiting for what happens next.
    loop _ _ Abort _ _ = handOver wd
    loop n prev action budget0 deadline0 = do
        (budget, deadline) <- apply action budget0 deadline0
        ev <- waitFor (Just n) =<< remaining deadline
        case ev of
            Stop -> return ()
            Changed -> do
                -- However busy the task is, look at it once per
                -- interval at most: recording costs a write to a 'TVar'
                -- and nothing else, and what the task has settled
                -- on is what 'decide' is asked about.  The changes are not
                -- waited for here, or the first of them would end the wait.
                quiet <- waitFor Nothing $ Just $ interval budget
                case quiet of
                    Stop -> return ()
                    _ -> do
                        Recorded n' curr <- readTVarIO $ wdRecorded wd
                        action' <- decide (Just prev) curr
                        loop n' curr action' budget deadline
            Expired -> do
                atomically $ writeTVar (wdTimedOut wd) True
                -- The same interval is how long the task is given
                -- to finish by itself before it is given up on, whatever it
                -- does meanwhile.
                quiet <- waitFor Nothing $ Just $ interval budget
                case quiet of
                    Stop -> return ()
                    _ -> lastResort

    apply action budget deadline = do
        now <- getNow
        return $ case action of
            Ignore -> (budget, deadline)
            Unlimited -> (budget, Nothing)
            SetTimeout us -> (Just us, Just $ now + us * 1000)
            TimedOut -> (budget, Just now)
            Abort -> (budget, deadline)

    -- A quarter of what is being waited out, capped at one second, and
    -- one second when nothing is.  In microseconds.
    interval :: Maybe Word64 -> Int
    interval = maybe 1000000 (fromIntegral . max 1 . min 1000000 . (`div` 4))

    -- How long there is left until the deadline, in microseconds.
    remaining :: Maybe Word64 -> IO (Maybe Int)
    remaining Nothing = return Nothing
    remaining (Just d) = do
        now <- getNow
        -- Subtracting 'Word64' wraps, so a deadline already passed is the
        -- shortest wait there is, not the longest.
        return $ Just $ if now >= d then 1 else toMicros (d - now)

    -- If the task moved and the timer expired at the same time, it
    -- moved: a task which turns out to be alive is not given up on.
    waitFor :: Maybe Int -> Maybe Int -> IO Event
    waitFor mseen mt =
        withTimer mt $ \expired ->
            atomically $ stopped <|> handedOver <|> changed <|> timeUp expired
      where
        stopped = readTVar done >>= check >> return Stop
        -- Handed over: this watchdog is finished with the task,
        -- whatever it was about to decide.
        handedOver = readTVar (wdHandedOver wd) >>= check >> return Stop
        -- Changes since the count given, if one is.
        changed = case mseen of
            Nothing -> retry
            Just seen -> do
                Recorded n _ <- readTVar $ wdRecorded wd
                check $ n /= seen
                return Changed
        timeUp expired = expired >> return Expired

-- | Monotonic time in nanoseconds.
getNow :: IO Word64
getNow = getMonotonicTimeNSec

-- | Nanoseconds as the microseconds the timer takes, which is an 'Int'.
--   What does not fit is longer than any timeout worth setting.
toMicros :: Word64 -> Int
toMicros ns = fromIntegral $ max 1 $ min cap $ ns `div` 1000
  where
    cap = fromIntegral (maxBound :: Int)

----------------------------------------------------------------

-- | Running a body with an STM action which succeeds once the time
--   passed. The timer is cancelled when the body finishes.
withTimer :: Maybe Int -> (STM () -> IO a) -> IO a
withTimer Nothing body = body retry
#if defined(mingw32_HOST_OS)
withTimer (Just t) body = do
    var <- registerDelay t
    body (readTVar var >>= check)
#else
withTimer (Just t) body = do
    var <- newTVarIO False
    mgr <- EV.getSystemTimerManager
    E.bracket
        (EV.registerTimeout mgr t $ atomically $ writeTVar var True)
        (EV.unregisterTimeout mgr)
        $ \_ -> body (readTVar var >>= check)
#endif
