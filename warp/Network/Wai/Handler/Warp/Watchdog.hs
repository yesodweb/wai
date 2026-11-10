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
import Data.Word (Word64)
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

-- | The watchdog of one connection warp serves itself, if it has a
--   timeout. With none there is no watchdog, and recording what the
--   connection does costs nothing, not even a transaction.
newtype ConnWatchdog = ConnWatchdog (Maybe (Watchdog Http1))

-- | Creating one with a timeout in microseconds. Zero or less means the
--   connection never times out.
newConnWatchdog :: Int -> IO ConnWatchdog
newConnWatchdog us
    | us <= 0 = return $ ConnWatchdog Nothing
    | otherwise =
        ConnWatchdog . Just
            <$> W.newWatchdog
                (Http1Context (fromIntegral us) $ Activity 0 0 False False False)

-- | Succeeding once the connection has timed out.
timedOutSTM :: ConnWatchdog -> STM ()
timedOutSTM (ConnWatchdog Nothing) = retry
timedOutSTM (ConnWatchdog (Just wd)) = W.timedOutSTM wd

-- | Whether the connection has timed out.
isTimedOut :: ConnWatchdog -> IO Bool
isTimedOut (ConnWatchdog Nothing) = return False
isTimedOut (ConnWatchdog (Just wd)) = W.isTimedOut wd

-- | Handing the connection to another owner. See 'W.handOver'.
handOver :: ConnWatchdog -> IO ()
handOver (ConnWatchdog mwd) = mapM_ W.handOver mwd

-- | Supervising a body with the watchdog thread. See 'W.withWatchdog'.
withWatchdog :: ConnWatchdog -> IO () -> IO a -> IO a
withWatchdog (ConnWatchdog Nothing) _ body = body
withWatchdog (ConnWatchdog (Just wd)) lastResort body =
    W.withWatchdog wd lastResort body

-- | The three flags are flags rather than counts because warp serves one
--   request at a time on an HTTP\/1.1 connection: one application runs,
--   and it reads or writes.  An application which writes from two threads
--   of its own -- a hijacked connection, say -- clears the flag when the
--   first of them is done, and its connection is then watched by whatever
--   rule comes next rather than by the one for a write in progress.
data Activity = Activity
    { actRx :: Int
    -- ^ Progress of the peer.
    , actTx :: Int
    -- ^ Completed writes.
    , actSending :: Bool
    -- ^ A write is in progress.
    , actWaiting :: Bool
    -- ^ A read on behalf of an application is in progress.
    , actApps :: Bool
    -- ^ An application is running.
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
    | actSending a = Writing $ actTx a
    | actWaiting a = ReadingForApp $ actRx a
    | actApps a = RunningApp
    | otherwise = Idling $ actRx a

instance WatchdogFor Http1 where
    data ContextFor Http1 = Http1Context
        { h1Timeout :: Word64
        , h1Activity :: Activity
        }

    decide mold new = return $ case mold of
        -- The watchdog starting: the connection has yet to do anything,
        -- and what it is not doing is already on the clock.
        Nothing -> action
        Just old
            | rule (h1Activity old) == r' -> ignore
            | otherwise -> action
      where
        r' = rule $ h1Activity new
        action
            | RunningApp <- r' = unlimited
            | otherwise = setTimeout $ h1Timeout new

modifyActivity :: ConnWatchdog -> (Activity -> Activity) -> IO ()
modifyActivity (ConnWatchdog Nothing) _ = return ()
modifyActivity (ConnWatchdog (Just wd)) f =
    update (\c -> c{h1Activity = f $ h1Activity c}) wd

-- | The peer made progress.
rxTick :: ConnWatchdog -> IO ()
rxTick wd = modifyActivity wd $ \a -> a{actRx = actRx a + 1}

-- | A write made progress, such as a part of a file sent.
txTick :: ConnWatchdog -> IO ()
txTick wd = modifyActivity wd $ \a -> a{actTx = actTx a + 1}

during :: (Bool -> Activity -> Activity) -> ConnWatchdog -> IO a -> IO a
during _ (ConnWatchdog Nothing) body = body
during set wd body =
    E.bracket_ (modifyActivity wd $ set True) (modifyActivity wd $ set False) body

-- | Running a write. 'txTick' is called when it completes.
sending :: ConnWatchdog -> IO a -> IO a
sending wd body = do
    r <- during (\b a -> a{actSending = b}) wd body
    txTick wd
    return r

-- | Running a read which waits for the peer on behalf of an
--   application, such as reading a request body.
waitingForPeer :: ConnWatchdog -> IO a -> IO a
waitingForPeer = during $ \b a -> a{actWaiting = b}

-- | Running an application.
runningApp :: ConnWatchdog -> IO a -> IO a
runningApp = during $ \b a -> a{actApps = b}
