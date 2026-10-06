{-# LANGUAGE TypeFamilies #-}

-- | What warp records about a connection it serves itself, and how long
--   it gives the peer to make progress.
--
--   The rules are these, in order:
--
--   1. A write in progress must make progress.
--
--   2. Otherwise, while a read is waited for on behalf of an
--      application, such as a request body, the peer must make progress.
--
--   3. Otherwise, while an application is running there is no limit: how
--      long a handler takes is the application's business.
--
--   4. Otherwise the connection is idle and the peer must make progress.
--
--   Progress is what is reported with 'rxTick' (the peer) and 'sending'
--   or 'txTick' (a write). Under rules 1, 2 and 4, progress restarts the
--   timer, and so does moving from one rule to another.
--
--   An HTTP\/2 connection is not watched here: warp hands it to the
--   http2 library, which supervises it with a watchdog of its own.
module Network.Wai.Handler.Warp.Watchdog (
    Http1,
    ConnWatchdog,
    newConnWatchdog,

    -- * Recording activity
    rxTick,
    txTick,
    sending,
    waitingForPeer,
    runningApp,

    -- * The rest of the watchdog, for a connection of warp's
    timedOutSTM,
    isTimedOut,
    handOver,
    withWatchdog,
) where

import Control.Concurrent.STM (STM, retry)
import qualified Control.Exception as E
import Control.Monad (when)
import System.Watchdog hiding (
    handOver,
    isTimedOut,
    newWatchdog,
    timedOutSTM,
    withWatchdog,
 )
import qualified System.Watchdog as W

-- | Warp's own part of a connection: HTTP\/1.1, and whatever comes
--   before warp knows which protocol it is.
data Http1

-- | The watchdog of one connection warp serves itself.
--
--   With no timeout there is nothing to decide, and recording what the
--   connection does costs nothing: the flag is here, rather than in the
--   context, so that a tick on a hot path is not even a transaction.
data ConnWatchdog = ConnWatchdog !Bool !(Watchdog Http1)

-- | Creating one with a timeout in microseconds. Zero or less means the
--   connection never times out.
newConnWatchdog :: Int -> IO ConnWatchdog
newConnWatchdog us = do
    wd <- W.newWatchdog $ Http1Context us $ Activity 0 0 0 0 0
    -- Nothing to watch, so no thread watches it.
    when (us <= 0) $ W.handOver wd
    return $ ConnWatchdog (us > 0) wd

-- | Succeeding once the connection has timed out.
timedOutSTM :: ConnWatchdog -> STM ()
timedOutSTM (ConnWatchdog False _) = retry
timedOutSTM (ConnWatchdog True wd) = W.timedOutSTM wd

-- | Whether the connection has timed out.
isTimedOut :: ConnWatchdog -> IO Bool
isTimedOut (ConnWatchdog False _) = return False
isTimedOut (ConnWatchdog True wd) = W.isTimedOut wd

-- | Handing the connection to another owner. See 'W.handOver'.
handOver :: ConnWatchdog -> IO ()
handOver (ConnWatchdog _ wd) = W.handOver wd

-- | Supervising an action with the watchdog thread. See 'W.withWatchdog'.
withWatchdog :: ConnWatchdog -> IO () -> IO a -> IO a
withWatchdog (ConnWatchdog False _) _ action = action
withWatchdog (ConnWatchdog True wd) lastResort action =
    W.withWatchdog wd lastResort action

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

-- | What the connection must do to stay alive. Two 'Rule's differ when
--   the connection made progress which counts, or moved on.
data Rule
    = Writing Int
    | ReadingForApp Int
    | RunningApp
    | Idling Int
    deriving (Eq)

rule :: Activity -> Rule
rule a
    | actSending a > 0 = Writing $ actTx a
    | actWaiting a > 0 = ReadingForApp $ actRx a
    | actApps a > 0 = RunningApp
    | otherwise = Idling $ actRx a

instance WatchdogFor Http1 where
    data ContextFor Http1 = Http1Context
        { h1Timeout :: Int
        , h1Activity :: Activity
        }

    decide mold new = return $ case mold of
        -- The watchdog starting: the connection has yet to do anything,
        -- and what it is not doing is already on the clock.
        Nothing -> act
        Just old
            | rule (h1Activity old) == r' -> Ignore
            | otherwise -> act
      where
        r' = rule $ h1Activity new
        act
            | RunningApp <- r' = Unlimited
            | otherwise = setTimeout $ h1Timeout new

modifyActivity :: ConnWatchdog -> (Activity -> Activity) -> IO ()
modifyActivity (ConnWatchdog False _) _ = return ()
modifyActivity (ConnWatchdog True wd) f =
    update (\c -> c{h1Activity = f $ h1Activity c}) wd

-- | The peer made progress.
rxTick :: ConnWatchdog -> IO ()
rxTick wd = modifyActivity wd $ \a -> a{actRx = actRx a + 1}

-- | A write made progress, such as a part of a file sent.
txTick :: ConnWatchdog -> IO ()
txTick wd = modifyActivity wd $ \a -> a{actTx = actTx a + 1}

during
    :: (Activity -> Activity)
    -> (Activity -> Activity)
    -> ConnWatchdog
    -> IO a
    -> IO a
during _ _ (ConnWatchdog False _) act = act
during begin end wd act =
    E.bracket_ (modifyActivity wd begin) (modifyActivity wd end) act

-- | Running a write. 'txTick' is called when it completes.
sending :: ConnWatchdog -> IO a -> IO a
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
waitingForPeer :: ConnWatchdog -> IO a -> IO a
waitingForPeer =
    during
        (\a -> a{actWaiting = actWaiting a + 1})
        (\a -> a{actWaiting = actWaiting a - 1})

-- | Running an application.
runningApp :: ConnWatchdog -> IO a -> IO a
runningApp =
    during
        (\a -> a{actApps = actApps a + 1})
        (\a -> a{actApps = actApps a - 1})
