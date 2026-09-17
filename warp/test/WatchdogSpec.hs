{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WatchdogSpec (spec) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Concurrent.STM (readTVarIO)
import qualified Control.Exception as E
import Control.Monad (forM_, forever)
import qualified Data.ByteString as S
import Data.ByteString.Builder (byteString)
import Data.IORef
import Data.Streaming.Network (bindPortTCP, getSocketTCP, safeRecv)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import Network.HTTP.Types (status200)
import Network.Socket (Socket, close)
import Network.Socket.ByteString (sendAll)
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO.Error (isUserError)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.Hspec

import Network.Wai.Handler.Warp.Watchdog

-- | Timeout used throughout, in microseconds. Small enough to keep the suite
-- quick, large enough that the grace and rate-limit sleeps it is clamped
-- against (a quarter of it, so 25ms each) stay well clear of scheduler noise.
--
-- A stalled connection is therefore killed at about
-- @grace + rateLimit + budget == 150ms@.
budget :: Int
budget = 100_000

-- | Run a watchdog with its own supervision var, the way 'withConnWatchdog'
-- does. 'Nothing' means it was told to stop rather than seeing a stall.
runWatchdog :: Int -> ConnState -> IO Phase
runWatchdog us cs = watchdog us cs

spec :: Spec
spec = do
    describe "phaseBudget" $ do
        it "leaves application time, and only application time, unlimited" $ do
            phaseBudget budget RunningApp `shouldBe` NoLimit
            phaseBudget budget Delegated `shouldBe` NoLimit

        it "caps header reading outright, but lets the other phases extend" $ do
            -- A Total budget on headers is the slowloris rule.
            phaseBudget budget ReadingHeaders `shouldBe` Total budget
            -- Uploads and responses may run as long as they keep moving.
            phaseBudget budget ReadingBody `shouldBe` Extends budget
            phaseBudget budget SendingResponse `shouldBe` Extends budget

    describe "tick/enter" $ do
        it "tick counts progress and leaves the phase alone" $ do
            cs <- newConnState
            enter cs SendingResponse
            tick cs
            tick cs
            TaggedPhase n p <- readTVarIO cs
            p `shouldBe` SendingResponse
            n `shouldBe` 3

        it "enter counts as progress too" $ do
            cs <- newConnState
            TaggedPhase n0 p0 <- readTVarIO cs
            p0 `shouldBe` ReadingHeaders
            enter cs RunningApp
            TaggedPhase n1 p1 <- readTVarIO cs
            p1 `shouldBe` RunningApp
            n1 `shouldBe` n0 + 1

    describe "watchdog" $ do
        it "returns the phase that overran" $ do
            cs <- newConnState
            enter cs SendingResponse
            runWatchdog budget cs `shouldReturn` SendingResponse

        it "does not fire while an extending phase keeps making progress" $ do
            cs <- newConnState
            enter cs SendingResponse
            ticker <- forkIO $ forever $ threadDelay 10_000 >> tick cs
            r <- timeout (budget * 5) $ runWatchdog budget cs
            killThread ticker
            r `shouldBe` Nothing

        it "fires on a Total phase however busy it is" $ do
            -- The slowloris rule. ReadingHeaders has a Total budget, so
            -- unlike the case above, ticking buys no extra time at all.
            cs <- newConnState
            enter cs ReadingHeaders
            ticker <- forkIO $ forever $ threadDelay 10_000 >> tick cs
            p <- runWatchdog budget cs
            killThread ticker
            p `shouldBe` ReadingHeaders

        it "stops the clock on a Total phase once the phase is left" $ do
            cs <- newConnState
            enter cs ReadingHeaders
            -- Headers complete inside the cap, then the application runs long.
            switcher <- forkIO $ threadDelay (budget `div` 2) >> enter cs RunningApp
            r <- timeout (budget * 5) $ runWatchdog budget cs
            killThread switcher
            r `shouldBe` Nothing

        it "never fires in a phase with no timeout" $ do
            cs <- newConnState
            enter cs RunningApp
            r <- timeout (budget * 5) $ runWatchdog budget cs
            r `shouldBe` Nothing

        it "starts timing again when a timed phase is re-entered" $ do
            cs <- newConnState
            enter cs RunningApp
            switcher <- forkIO $ threadDelay budget >> enter cs SendingResponse
            runWatchdog budget cs `shouldReturn` SendingResponse
            killThread switcher

        it "lets a change cancel the timer in flight, restoring the full budget" $ do
            cs <- newConnState
            enter cs SendingResponse
            -- One nudge, landing clearly inside the armed budget rather than
            -- at its edge, so this does not race the expiry it is testing.
            nudger <- forkIO $ threadDelay (budget `div` 2) >> tick cs
            start <- getMonotonicTimeNSec
            p <- runWatchdog budget cs
            elapsed <- subtract start <$> getMonotonicTimeNSec
            killThread nudger
            p `shouldBe` SendingResponse
            -- Unnudged this fires at ~100ms. The nudge has to cancel that
            -- timer and buy a fresh budget: 50ms in, then a rate-limit sleep,
            -- then a full budget again, so comfortably past 150ms.
            elapsed `shouldSatisfy` (> 150_000_000)

    describe "withConnWatchdog" $ do
        it "leaves a connection that finishes inside the grace period alone" $ do
            finished <- newIORef False
            withConnWatchdog budget $ \cs -> do
                enter cs SendingResponse
                threadDelay $ budget `div` 8
                writeIORef finished True
            readIORef finished `shouldReturn` True

        it "interrupts a stalled connection and swallows TimeoutThread" $ do
            finished <- newIORef False
            withConnWatchdog budget $ \cs -> do
                enter cs SendingResponse
                threadDelay $ budget * 50
                writeIORef finished True
            readIORef finished `shouldReturn` False

        it "does not swallow other exceptions" $
            withConnWatchdog budget (\_ -> E.throwIO $ userError "boom")
                `shouldThrow` isUserError

        it "starts no watchdog at all when timeouts are disabled" $ do
            finished <- newIORef False
            withConnWatchdog 0 $ \cs -> do
                enter cs SendingResponse
                threadDelay $ budget * 5
                writeIORef finished True
            readIORef finished `shouldReturn` True

    -- The two cases the phase split exists to tell apart, checked against a
    -- real server over a real socket rather than against the watchdog alone.
    describe "against a running server (settingsTimeout = 1s)" $ do
        it "kills a client that stops mid-headers" $
            withServer slowServer helloApp $ \port ->
                E.bracket (fst <$> getSocketTCP "127.0.0.1" port) close $ \s -> do
                    -- A request line and one header, but never the blank line
                    -- that would end them. Warp stays in ReadingHeaders.
                    sendAll s "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\n"
                    expectClosedWithin s 4_000_000_000

        it "kills a client that dribbles headers fast enough to beat the old size rule" $
            withServer slowServer helloApp $ \port ->
                E.bracket (fst <$> getSocketTCP "127.0.0.1" port) close $ \s -> do
                    sendAll s "GET / HTTP/1.1\r\n"
                    -- 2KB of well-formed header every 300ms, never finishing.
                    -- Each write clears the old settingsSlowlorisSize bar of
                    -- 2048 bytes, so under the previous rule every one of them
                    -- refreshed the timer and this client could have held the
                    -- connection open indefinitely. The Total budget on
                    -- ReadingHeaders ends it regardless of chunk size.
                    feeder <- forkIO $ forever $ do
                        sendAll s $ "X-Pad: " <> S.replicate 2048 0x61 <> "\r\n"
                        threadDelay 300_000
                    expectClosedWithin s 4_000_000_000
                    killThread feeder

        it "does not kill a streaming body that pauses longer than the timeout" $
            withServer slowServer dribbleApp $ \port ->
                E.bracket (fst <$> getSocketTCP "127.0.0.1" port) close $ \s -> do
                    sendAll s "GET / HTTP/1.1\r\nHost: 127.0.0.1\r\n\r\n"
                    -- Two chunks, 1.4s apart, on a 1s timeout: every gap is
                    -- longer than the whole budget, and 'RunningApp' is what
                    -- makes this survive.
                    --
                    -- Warp already got this right on HTTP/1.1 by pausing the
                    -- timer around each fragment, so this is a regression
                    -- guard rather than a fix. It is here because it is the
                    -- property most easily lost when the pause/resume pair is
                    -- re-expressed as phases. (The SSE timeout reported in
                    -- kazu-yamamoto/http2#173 is the HTTP/2 per-stream timer,
                    -- which this change does not touch.)
                    body <- readUntil s 2 mempty
                    S.count 0x21 body `shouldBe` 2 -- '!' , one per chunk

-- | Block until the server hangs up, and assert it did so in time.
expectClosedWithin :: Socket -> Word64 -> IO ()
expectClosedWithin s limit = do
    start <- getMonotonicTimeNSec
    r <- E.try $ drain
    elapsed <- subtract start <$> getMonotonicTimeNSec
    case r :: Either E.SomeException () of
        Left _ -> return () -- connection reset counts as a kill
        Right () -> return ()
    elapsed `shouldSatisfy` (< limit)
  where
    -- Read until EOF. A dribbling client may be sent an error response first.
    drain = do
        bs <- safeRecv s 4096
        if S.null bs then return () else drain

helloApp :: Application
helloApp _ respond = respond $ responseLBS status200 [] "hello"

-- | Thinks for well over the timeout before its first chunk, then again
-- between chunks. The first gap is the one that matters: the response headers
-- are usually small enough to stay in the builder's buffer, so no fragment is
-- written before the application is handed control, and nothing else would
-- move the connection out of 'SendingResponse'.
dribbleApp :: Application
dribbleApp _ respond =
    respond $ responseStream status200 [] $ \write flush -> do
        threadDelay 2_500_000
        write (byteString "!") >> flush
        threadDelay 1_400_000
        write (byteString "!") >> flush

slowServer :: Settings
slowServer = setTimeout 1 defaultSettings

-- | Read until @n@ exclamation marks have arrived, or give up.
readUntil :: Socket -> Int -> S.ByteString -> IO S.ByteString
readUntil s n acc
    | S.count 0x21 acc >= n = return acc
    | otherwise = do
        r <- timeout 6_000_000 $ safeRecv s 4096
        case r of
            Nothing -> return acc
            Just bs
                | S.null bs -> return acc
                | otherwise -> readUntil s n (acc <> bs)

withServer :: Settings -> Application -> (Int -> IO a) -> IO a
withServer settings app f = do
    port <- freePort
    baton <- newEmptyMVar
    let settings' =
            setPort port $
                setHost "127.0.0.1" $
                    setBeforeMainLoop (putMVar baton ()) settings
    E.bracket
        (forkIO $ runSettings settings' app `E.onException` putMVar baton ())
        killThread
        (const $ takeMVar baton >> f port)

nextPort :: IORef Int
nextPort = unsafePerformIO $ newIORef 5700
{-# NOINLINE nextPort #-}

freePort :: IO Int
freePort = do
    port <- atomicModifyIORef' nextPort $ \p -> (p + 1, p)
    r <- E.try $ bindPortTCP port "127.0.0.1"
    case r :: Either E.IOException Socket of
        Left _ -> freePort
        Right sock -> close sock >> return port
