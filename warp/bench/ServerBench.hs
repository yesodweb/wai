{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

-- | End-to-end benchmark of a real Warp server over real sockets.
--
-- Warp's other benchmarks measure pieces in isolation: @bench:parser@ parses
-- request lines, and @bench:response@ drives 'sendResponse' against a sink
-- @Connection@ that never touches a socket. Neither accepts a connection,
-- forks a thread, or writes a byte, so neither can say what a /connection/
-- costs — which is the question for timeout handling, thread-per-connection
-- overhead, and anything else that scales with connection count rather than
-- with request count.
--
-- == How it works
--
-- The binary runs as two processes. Invoked normally it is the driver;
-- invoked with @--serve@ it is the server. The driver re-execs itself, reads
-- the port the child chose from the child's stdout, applies load, and samples
-- the child's CPU and memory from @\/proc@.
--
-- Two processes rather than one because the numbers that matter are the
-- /server's/ CPU and memory, and those are meaningless if the load generator
-- shares the process with it.
--
-- == Usage
--
-- > cabal bench warp:bench:server \
-- >   --benchmark-options='--scenario keepalive --conns 100 --duration 10'
--
-- [@keepalive@] Connections issue requests back to back. Amortises
--   per-connection cost, so it isolates the per-request hot path.
-- [@churn@] Connect, one request, close. Exposes per-connection cost.
-- [@idle@] Open many connections and hold them. Exposes per-connection
--   memory and thread cost.
--
--   @--arrival-rate@ matters here and is worth setting deliberately.
--   Unpaced, this establishes tens of thousands of connections per second,
--   which models a reconnect storm after a deploy or network partition, not
--   steady state: any per-connection work a timeout scheme defers will
--   synchronise and contend. Real persistent connections arrive far slower --
--   a TLS handshake costs roughly a core-millisecond, so a TLS server cannot
--   accept anywhere near that rate regardless. Both regimes are worth
--   measuring; they are different questions.
--
--   Caution on @total cpu ns per conn@ at low arrival rates: pacing 10k
--   connections at 100\/s stretches the establishment window to 100 seconds,
--   and whatever CPU the server burns per second of simply existing is then
--   divided across the connections and folded into that figure. Compare
--   totals only between runs at the same arrival rate.
-- [@stream@] Responses of many chunked fragments. Exposes per-chunk cost.
--
-- == Comparing two versions of Warp
--
-- This is the only way the benchmark can inform a decision, so it is worth
-- doing properly.
--
-- The numbers are meaningless in isolation. Throughput and per-connection CPU
-- depend on core count, kernel version, and whatever else the machine is
-- doing. The figure that carries information is the /ratio/ between two builds
-- measured on one machine in one sitting; an absolute req\/s quoted across
-- machines says nothing about the code. A comparison therefore means two
-- builds, because this measures whichever Warp it was linked against and
-- cannot switch at runtime.
--
-- Three things separate a comparison from a coin toss:
--
-- [Copy this file into the baseline tree] The benchmark lives in the tree
--   being measured, so an older branch does not have it and
--   @cabal build warp:bench:server@ there simply fails. Copy
--   @bench\/ServerBench.hs@ /and/ the @benchmark server@ stanza from
--   @warp.cabal@ into the baseline worktree, unchanged. If the two trees run
--   different benchmark source -- a different warmup, a different response
--   size, a different scenario definition -- the result measures the
--   benchmark rather than Warp.
--
-- [Interleave and take medians] Run A, B, A, B, A, B; not A three times then
--   B three times. Per-connection CPU has been seen to move 35% between
--   consecutive runs of the same binary on an idle machine. Running each
--   build as one block attributes all of that drift to the code difference,
--   and 35% is larger than most differences worth arguing about.
--
-- [Use an idle machine] A background backup job was once enough to make this
--   benchmark's variance exceed its signal entirely.
--
-- All of that is what @bench\/compare.sh@ does, so the short version is:
--
-- > warp/bench/compare.sh origin/master origin/some-branch .
--
-- where @.@ means the current working tree. It builds each version, runs the
-- scenarios interleaved, and prints medians. By hand it is:
--
-- > # one worktree per version under test
-- > git worktree add --detach /tmp/base origin/master
-- >
-- > # give it this benchmark and its cabal stanza. Cabal does not care where
-- > # a stanza appears, so appending avoids hand-editing the file.
-- > cp warp/bench/ServerBench.hs /tmp/base/warp/bench/
-- > sed -n '/^benchmark server$/,/^benchmark response$/p' warp/warp.cabal \
-- >   | sed '$d' >> /tmp/base/warp/warp.cabal
-- > (cd /tmp/base && cabal build warp:bench:server)
-- > cabal build warp:bench:server
-- >
-- > BASE=$(cd /tmp/base && cabal list-bin warp:bench:server)
-- > MINE=$(cabal list-bin warp:bench:server)
-- >
-- > rm -f base.json mine.json        # or a re-run silently appends
-- > for i in 1 2 3; do               # interleaved, not one block each
-- >   "$BASE" --scenario churn --json >> base.json
-- >   "$MINE" --scenario churn --json >> mine.json
-- > done
--
-- Take the median of each metric across rounds. Three versions is no
-- different from two; keep them all in the same interleaved loop.
--
-- A baseline that changes @time-manager@\'s version needs
-- @allow-newer: http2:time-manager, http3:time-manager, http-semantics:time-manager@
-- in a @cabal.project.local@ inside its worktree, or dependency resolution
-- fails before anything is built.
--
-- Finally, mind what is comparable. Figures normalised per connection are
-- comparable only between runs with the same @--arrival-rate@, and per-request
-- CPU is inflated at low load because idle GHC capabilities spin, so check
-- that throughput has saturated before reading anything into it.
module Main (main) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar
import Control.Exception (IOException, bracket, try)
import Control.Monad (forM, forM_, replicateM_, unless, void, when)
import qualified Data.ByteString as S
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as C8
import Data.Array.IO (IOUArray)
import Data.Array.MArray (getBounds, newArray, readArray, writeArray)
import Data.IORef
import Data.List (intercalate, isPrefixOf)
import qualified Data.Text as T
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc (getNumCapabilities)
import Network.HTTP.Types (status200)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai
import Network.Wai.Handler.Warp
import System.Directory (doesFileExist)
import System.Environment (getArgs, getExecutablePath)
import System.CPUTime (getCPUTime)
import System.Exit (exitFailure)
import System.IO
import System.Process

#if !defined(mingw32_HOST_OS)
import System.Posix.Unistd (SysVar (ClockTick), getSysVar)
#endif

----------------------------------------------------------------
-- Configuration

data Scenario = SKeepAlive | SChurn | SIdle | SStream
    deriving (Eq, Show)

data Config = Config
    { cfgScenario :: Scenario
    , cfgConns :: Int
    , cfgDuration :: Int
    -- ^ Seconds of measurement; for 'SIdle', seconds to hold the connections.
    , cfgWarmup :: Int
    , cfgTotal :: Int
    -- ^ Total connections for 'SChurn', which is count-bounded rather than
    --   time-bounded so it stays inside the ephemeral port range. The default
    --   is chosen to give a measured window of several seconds: CPU is
    --   sampled at clock-tick granularity, so a sub-second run cannot report
    --   a trustworthy cpu\/op.
    , cfgFragments :: Int
    , cfgTimeout :: Int
    -- ^ The server's 'setTimeout', in seconds.
    , cfgServerRts :: String
    , cfgArrivalRate :: Int
    -- ^ For 'SIdle': connections per second to establish, or 0 for as fast as
    --   possible. Real servers reach a large persistent-connection count over
    --   minutes, not milliseconds, and establishing them all at once
    --   synchronises whatever per-connection work a timeout scheme defers.
    , cfgJson :: Bool
    }

defaultConfig :: Config
defaultConfig =
    Config
        { cfgScenario = SKeepAlive
        , cfgConns = 100
        , cfgDuration = 10
        , cfgWarmup = 2
        , cfgTotal = 150000
        , cfgFragments = 64
        , -- Long enough that 'SIdle' is not reaped by the very mechanism under
          -- test.
          cfgTimeout = 300
        , cfgServerRts = "-N4"
        , cfgArrivalRate = 0
        , cfgJson = False
        }

parseArgs :: [String] -> Config -> Either String Config
parseArgs [] c = Right c
parseArgs ("--json" : r) c = parseArgs r c{cfgJson = True}
parseArgs ("--scenario" : v : r) c = do
    s <- case v of
        "keepalive" -> Right SKeepAlive
        "churn" -> Right SChurn
        "idle" -> Right SIdle
        "stream" -> Right SStream
        _ -> Left $ "unknown scenario: " ++ v
    parseArgs r c{cfgScenario = s}
parseArgs ("--conns" : v : r) c = parseArgs r c{cfgConns = read v}
parseArgs ("--duration" : v : r) c = parseArgs r c{cfgDuration = read v}
parseArgs ("--warmup" : v : r) c = parseArgs r c{cfgWarmup = read v}
parseArgs ("--total" : v : r) c = parseArgs r c{cfgTotal = read v}
parseArgs ("--fragments" : v : r) c = parseArgs r c{cfgFragments = read v}
parseArgs ("--timeout" : v : r) c = parseArgs r c{cfgTimeout = read v}
parseArgs ("--server-rts" : v : r) c = parseArgs r c{cfgServerRts = v}
parseArgs ("--arrival-rate" : v : r) c = parseArgs r c{cfgArrivalRate = read v}
parseArgs (x : _) _ = Left $ "unknown option: " ++ x

usage :: String
usage =
    unlines
        [ "warp end-to-end server benchmark"
        , ""
        , "  --scenario keepalive|churn|idle|stream"
        , "  --conns N          concurrent connections (default 100)"
        , "  --duration SECS    measurement window, or hold time for idle (default 10)"
        , "  --warmup SECS      discarded before measuring (default 2)"
        , "  --total N          total connections for churn (default 150000)"
        , "  --fragments N      fragments per response for stream (default 64)"
        , "  --timeout SECS     the server's setTimeout (default 300)"
        , "  --server-rts OPTS  RTS options for the server process (default -N4)"
        , "  --arrival-rate N   idle: establish N conns/sec (default 0 = unpaced)"
        , "  --json             machine-readable output"
        ]

----------------------------------------------------------------

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case args of
        ("--serve" : rest) -> server rest
        ("--help" : _) -> putStr usage
        _ -> case parseArgs args defaultConfig of
            Left e -> hPutStrLn stderr (e ++ "\n\n" ++ usage) >> exitFailure
            Right cfg -> driver cfg

----------------------------------------------------------------
-- The server process

-- | The application under test. One server serves every scenario; the path
-- selects the shape of the response.
benchApp :: Application
benchApp req respond = case pathInfo req of
    ("stream" : n : _) ->
        respond $
            responseStream status200 [("Content-Type", "text/plain")] $
                \write flush ->
                    replicateM_ (readIntDef 64 n) (write (byteString "x") >> flush)
    -- A streaming response that thinks for a while before its first chunk.
    -- Warp is supposed to put no timeout on application think time; this is
    -- the shape that finds out whether it does.
    ("slowstream" : ms : n : _) ->
        respond $
            responseStream status200 [("Content-Type", "text/plain")] $
                \write flush -> do
                    threadDelay (readIntDef 0 ms * 1000)
                    replicateM_ (readIntDef 1 n) (write (byteString "x") >> flush)
    _ -> respond $ responseLBS status200 [("Content-Type", "text/plain")] "Hello, World!"
  where
    readIntDef d t = case reads (T.unpack t) of
        [(i, "")] -> i
        _ -> d

server :: [String] -> IO ()
server args = do
    let timeoutSecs = case args of
            (t : _) -> read t
            _ -> 300
    (port, sock) <- openFreePort
    caps <- getNumCapabilities
    -- The port handshake goes over stdout, so the driver never has to guess a
    -- port and race the bind.
    putStrLn $ "READY " ++ show port ++ " " ++ show caps
    hFlush stdout
    let set =
            setTimeout timeoutSecs $
                -- Clients vanishing mid-connection is the point of some of
                -- these scenarios, not an error worth printing.
                setOnException (\_ _ -> return ()) defaultSettings
    runSettingsSocket set sock benchApp

----------------------------------------------------------------
-- The driver process

driver :: Config -> IO ()
driver cfg = withServer cfg $ \port mpid caps -> do
    r <- case cfgScenario cfg of
        SKeepAlive -> runKeepAlive cfg port mpid
        SStream -> runStream cfg port mpid
        SChurn -> runChurn cfg port mpid
        SIdle -> runIdle cfg port mpid
    report cfg caps r

-- | Start the server as a child process, hand its port to the body, and make
-- sure it dies afterwards.
withServer :: Config -> (Int -> Maybe Pid -> Int -> IO a) -> IO a
withServer cfg body = do
    self <- getExecutablePath
    let rts = words (cfgServerRts cfg)
        childArgs =
            ["--serve", show (cfgTimeout cfg)]
                ++ (if null rts then [] else "+RTS" : rts ++ ["-RTS"])
    bracket (createProcess (proc self childArgs){std_out = CreatePipe}) cleanup $
        \(_, mout, _, ph) -> do
            out <- maybe (fail "no stdout pipe from server") return mout
            ready <- hGetLine out
            (port, caps) <- case words ready of
                ["READY", p, c] -> return (read p, read c)
                _ -> fail $ "server did not come up: " ++ show ready
            -- Drain the rest of the child's stdout so a long run can never
            -- block it on a full pipe.
            _ <- forkIO $ void $ try @IOException $ do
                s <- hGetContents out
                length s `seq` return ()
            mpid <- getPid ph
            body port mpid caps
  where
    cleanup (_, _, _, ph) = terminateProcess ph >> void (waitForProcess ph)

----------------------------------------------------------------
-- Client primitives

connectLocal :: Int -> IO Socket
connectLocal port = do
    let hints = defaultHints{addrSocketType = Stream, addrFlags = [AI_NUMERICHOST]}
    addrs <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just (show port))
    case addrs of
        [] -> fail "no address for 127.0.0.1"
        (addr : _) -> do
            s <- socket (addrFamily addr) Stream defaultProtocol
            -- Without this, small request writes are delayed by Nagle and we
            -- would be measuring the kernel's timer, not the server.
            setSocketOption s NoDelay 1
            connect s (addrAddress addr)
            return s

getReq :: Bool -> S.ByteString -> S.ByteString
getReq keepAlive path =
    S.concat
        [ "GET "
        , path
        , " HTTP/1.1\r\nHost: 127.0.0.1\r\n"
        , if keepAlive then "" else "Connection: close\r\n"
        , "\r\n"
        ]

-- | Read exactly @n@ bytes, failing on early EOF.
readExactly :: Socket -> Int -> IO ()
readExactly _ 0 = return ()
readExactly s n = do
    bs <- recv s (min 65536 n)
    if S.null bs
        then ioError (userError "unexpected EOF")
        else readExactly s (n - S.length bs)

-- | Read until the peer closes, returning how many bytes arrived.
readToEof :: Socket -> IO Int
readToEof s = go 0
  where
    go !acc = do
        bs <- recv s 65536
        if S.null bs then return acc else go (acc + S.length bs)

-- | Read one chunked response, through its terminating zero-length chunk.
readChunked :: Socket -> IO ()
readChunked s = go S.empty
  where
    terminator = "0\r\n\r\n"
    go tl = do
        bs <- recv s 65536
        when (S.null bs) $ ioError (userError "unexpected EOF in chunked body")
        let buf = tl <> bs
        if terminator `S.isInfixOf` buf
            then return ()
            else -- Keep the last few bytes so a terminator split across two
            -- reads is still found.
                go (S.drop (S.length buf - (S.length terminator - 1)) buf)

-- | Size of a complete keep-alive response, measured once so the hot loops
-- can count bytes instead of parsing HTTP on every request.
probeSize :: Int -> S.ByteString -> IO Int
probeSize port path = bracket (connectLocal port) close $ \s -> do
    sendAll s (getReq True path)
    readWholeResponse s

-- | Read one complete response and return its total size in bytes.
--
-- Handles both framings, because it matters which one you get: @responseLBS@
-- without an explicit @Content-Length@ makes Warp fall back to chunked
-- encoding, so a Content-Length-only reader blocks forever. Called once per
-- scenario, after which responses are a fixed size and the hot loops can just
-- count bytes instead of parsing HTTP on every request.
readWholeResponse :: Socket -> IO Int
readWholeResponse s = go S.empty
  where
    go acc =
        let (hdrs, rest) = S.breakSubstring "\r\n\r\n" acc
         in if S.null rest
                then more acc -- headers not complete yet
                else
                    let hdrLen = S.length hdrs + 4
                        body = S.drop hdrLen acc
                     in case contentLength hdrs of
                            Just len
                                | S.length body >= len -> return (hdrLen + len)
                                | otherwise -> more acc
                            Nothing
                                -- chunked: read through the terminating chunk
                                | "0\r\n\r\n" `S.isSuffixOf` acc -> return (S.length acc)
                                | otherwise -> more acc
    more acc = do
        bs <- recv s 65536
        if S.null bs then return (S.length acc) else go (acc <> bs)
    contentLength hdrs =
        case filter ("content-length:" `S.isPrefixOf`) (map lower (C8.lines hdrs)) of
            (l : _) -> fst <$> C8.readInt (C8.dropWhile (== ' ') (S.drop 15 l))
            [] -> Nothing
    lower = C8.map (\c -> if c >= 'A' && c <= 'Z' then toEnum (fromEnum c + 32) else c)

----------------------------------------------------------------
-- Concurrency helpers

now :: IO Word64
now = getMonotonicTimeNSec

secs :: Int -> Word64
secs n = fromIntegral n * 1000000000

-- | Run workers concurrently and collect what each returns.
inParallel :: Int -> (Int -> IO a) -> IO [a]
inParallel n act = do
    dones <- forM [0 .. n - 1] $ \i -> do
        d <- newEmptyMVar
        _ <- forkIO $ try @IOException (act i) >>= putMVar d
        return d
    rs <- mapM takeMVar dones
    forM rs $ either (fail . show) return

-- | An @n@-party barrier: workers 'arrive' and block; the driver 'waitAll's
-- until everyone has arrived, does something, then 'release's them.
newBarrier :: Int -> IO (IO (), IO (), IO ())
newBarrier n = do
    cnt <- newMVar (0 :: Int)
    allIn <- newEmptyMVar
    gate <- newEmptyMVar
    let arrive = do
            k <- modifyMVar cnt $ \x -> return (x + 1, x + 1)
            when (k == n) $ putMVar allIn ()
            readMVar gate
        waitAll = takeMVar allIn
        release = putMVar gate ()
    return (arrive, waitAll, release)

----------------------------------------------------------------
-- Latency histogram
--
-- A list of per-request timestamps costs the driver more GC than the server
-- costs to serve them, and the driver competes with the server for the same
-- cores. Microsecond-resolution buckets keep the client's per-request work to
-- one array bump.

newtype Hist = Hist (IOUArray Int Int)

-- | Microseconds. Anything slower lands in the top bucket.
histMax :: Int
histMax = 65536

newHist :: IO Hist
newHist = Hist <$> newArray (0, histMax) 0

record :: Hist -> Word64 -> IO ()
record (Hist a) ns = do
    let i = min histMax (fromIntegral (ns `div` 1000))
    v <- readArray a i
    writeArray a i (v + 1)

mergeHists :: [Hist] -> IO Hist
mergeHists hs = do
    out@(Hist o) <- newHist
    forM_ hs $ \(Hist a) -> do
        (lo, hi) <- getBounds a
        forM_ [lo .. hi] $ \i -> do
            x <- readArray a i
            when (x /= 0) $ readArray o i >>= \y -> writeArray o i (y + x)
    return out

histCount :: Hist -> IO Int
histCount (Hist a) = do
    (lo, hi) <- getBounds a
    go lo hi 0
  where
    go :: Int -> Int -> Int -> IO Int
    go i hi !acc
        | i > hi = return acc
        | otherwise = readArray a i >>= \x -> go (i + 1) hi (acc + x)

-- | Percentile in nanoseconds, to bucket resolution.
histPercentile :: Hist -> Double -> IO Word64
histPercentile h@(Hist a) p = do
    total <- histCount h
    let target = ceiling (p * fromIntegral total) :: Int
    (lo, hi) <- getBounds a
    go lo hi 0 target
  where
    go :: Int -> Int -> Int -> Int -> IO Word64
    go i hi !acc target
        | i > hi = return (fromIntegral histMax * 1000)
        | otherwise = do
            x <- readArray a i
            let acc' = acc + x
            if acc' >= target && target > 0
                then return (fromIntegral i * 1000)
                else go (i + 1) hi acc' target

-- | Repeat an action until a deadline, recording each iteration's latency.
untilDeadline :: Hist -> Word64 -> IO () -> IO ()
untilDeadline h deadline act = go
  where
    go = do
        t0 <- now
        when (t0 < deadline) $ do
            act
            t1 <- now
            record h (t1 - t0)
            go

----------------------------------------------------------------
-- Results

data Result = Result
    { rLabel :: String
    , rOps :: Int
    , rOpName :: String
    , rElapsedNs :: Word64
    , rHist :: Hist
    , rBefore :: Maybe ProcSample
    , rAfter :: Maybe ProcSample
    , rClientCpuNs :: Word64
    -- ^ The driver's own CPU over the same window. If this approaches the
    --   server's, the load generator is the bottleneck and the server numbers
    --   are a floor, not a measurement.
    , rExtra :: [(String, String)]
    }

-- | Process CPU of the driver itself, in nanoseconds.
clientCpu :: IO Word64
clientCpu = fromIntegral . (`div` 1000) <$> getCPUTime

----------------------------------------------------------------
-- Scenarios

-- | Shared shape of 'SKeepAlive' and 'SStream': persistent connections
-- repeating one operation. Warm-up runs first, then every worker waits at a
-- barrier so the CPU sample brackets exactly the measured window.
timedScenario
    :: Config
    -> Maybe Pid
    -> String
    -> String
    -> Int
    -- ^ ops per iteration (1 for requests, K for a K-fragment response)
    -> IO Socket
    -> (Socket -> IO ())
    -> IO Result
timedScenario cfg mpid label opName perIter mkConn op = do
    (arrive, waitAll, release) <- newBarrier (cfgConns cfg)
    warmEnd <- (+ secs (cfgWarmup cfg)) <$> now
    histVar <- newIORef Nothing
    started <- newEmptyMVar
    _ <- forkIO $ do
        hs <- inParallel (cfgConns cfg) $ \_ ->
            bracket mkConn close $ \s -> do
                warmH <- newHist
                untilDeadline warmH warmEnd (op s)
                arrive
                h <- newHist
                end <- (+ secs (cfgDuration cfg)) <$> now
                untilDeadline h end (op s)
                return h
        mergeHists hs >>= writeIORef histVar . Just
        putMVar started ()
    waitAll
    before <- readProc mpid
    ccpu0 <- clientCpu
    t0 <- now
    release
    takeMVar started
    t1 <- now
    ccpu1 <- clientCpu
    after <- readProc mpid
    Just h <- readIORef histVar
    n <- histCount h
    return
        Result
            { rLabel = label
            , rOps = n * perIter
            , rOpName = opName
            , rElapsedNs = t1 - t0
            , rHist = h
            , rBefore = before
            , rAfter = after
            , rClientCpuNs = ccpu1 - ccpu0
            , rExtra = []
            }

runKeepAlive :: Config -> Int -> Maybe Pid -> IO Result
runKeepAlive cfg port mpid = do
    sz <- probeSize port "/"
    r <-
        timedScenario cfg mpid "keepalive" "req" 1 (connectLocal port) $ \s ->
            sendAll s (getReq True "/") >> readExactly s sz
    return r{rExtra = [("response bytes", show sz)]}

runStream :: Config -> Int -> Maybe Pid -> IO Result
runStream cfg port mpid = do
    let path = C8.pack ("/stream/" ++ show (cfgFragments cfg))
    r <-
        timedScenario cfg mpid "stream" "fragment" (cfgFragments cfg) (connectLocal port) $
            \s -> sendAll s (getReq True path) >> readChunked s
    return
        r
            { rExtra =
                [ ("responses", show (rOps r `div` max 1 (cfgFragments cfg)))
                , ("fragments per response", show (cfgFragments cfg))
                ]
            }

runChurn :: Config -> Int -> Maybe Pid -> IO Result
runChurn cfg port mpid = do
    sz <- bracket (connectLocal port) close $ \s -> do
        sendAll s (getReq False "/")
        readToEof s
    let per = max 1 (cfgTotal cfg `div` cfgConns cfg)
        one = bracket (connectLocal port) close $ \s -> do
            sendAll s (getReq False "/")
            n <- readToEof s
            unless (n == sz) $ ioError (userError "short response")
    _ <- inParallel (cfgConns cfg) $ \_ -> replicateM_ 20 one
    before <- readProc mpid
    ccpu0 <- clientCpu
    t0 <- now
    hs <- inParallel (cfgConns cfg) $ \_ -> do
        h <- newHist
        forM_ [1 .. per] $ \_ -> do
            a <- now
            one
            b <- now
            record h (b - a)
        return h
    t1 <- now
    ccpu1 <- clientCpu
    after <- readProc mpid
    h <- mergeHists hs
    n <- histCount h
    return
        Result
            { rLabel = "churn"
            , rOps = n
            , rOpName = "conn"
            , rElapsedNs = t1 - t0
            , rHist = h
            , rBefore = before
            , rAfter = after
            , rClientCpuNs = ccpu1 - ccpu0
            , rExtra =
                [ ("response bytes", show sz)
                , ("note", "server closes, so TIME_WAIT accumulates server-side")
                ]
            }

-- | The 10k test: establish many connections, hold them idle, and see what
-- the server is carrying while it does so.
runIdle :: Config -> Int -> Maybe Pid -> IO Result
runIdle cfg port mpid = do
    sz <- probeSize port "/"
    before <- readProc mpid
    ccpu0 <- clientCpu
    t0 <- now
    held <- newIORef []
    hist <- newHist
    -- Establish in batches: the listen backlog is a few thousand, and firing
    -- all 10k connects at once would measure the accept queue, not the server.
    --
    -- With --arrival-rate, batches are also paced, because how fast
    -- connections arrive is itself a variable: a scheme that defers
    -- per-connection work will have that work synchronise when arrivals are
    -- synchronised, and 10k connections arriving in 400ms is a reconnect
    -- storm, not a steady state.
    let rate = cfgArrivalRate cfg
        batch
            | rate <= 0 = 500
            | otherwise = max 1 (min 500 (rate `div` 10))
        batches = chunksOf batch [1 .. cfgConns cfg]
    forM_ (zip [1 :: Int ..] batches) $ \(i, b) -> do
        when (rate > 0) $ do
            let dueNs = t0 + (fromIntegral (i * batch) * 1000000000) `div` fromIntegral rate
            nw <- now
            when (nw < dueNs) $ threadDelay (fromIntegral ((dueNs - nw) `div` 1000))
        void $ inParallel (length b) $ \_ -> do
            s <- connectLocal port
            a <- now
            sendAll s (getReq True "/")
            readExactly s sz
            c <- now
            record hist (c - a)
            atomicModifyIORef' held $ \ss -> (s : ss, ())
    t1 <- now
    -- What it costs the server merely to be holding these connections, with
    -- no traffic at all.
    --
    -- Measured in two equal windows rather than one, because a timeout scheme
    -- may do work once per connection some time after the connection is
    -- established -- starting a supervisor, arming a timer -- and averaging
    -- that one-off over a short hold reports it as though it were a rate. The
    -- first window catches any such transient; the second is steady state.
    -- Deliberately scheme-neutral: no knowledge of any particular grace period.
    early0 <- readProc mpid
    e0 <- now
    threadDelay (cfgDuration cfg * 1000000)
    e1 <- now
    early1 <- readProc mpid
    hold0 <- now
    threadDelay (cfgDuration cfg * 1000000)
    hold1 <- now
    holdTo <- readProc mpid
    let holdFrom = early1
    ccpu1 <- clientCpu
    after <- readProc mpid
    -- Confirm they are all still alive. A server that had reaped them would
    -- make the memory figure meaningless.
    ss <- readIORef held
    alive <- forM ss $ \s -> do
        r <- try @IOException (sendAll s (getReq True "/") >> readExactly s sz)
        return (either (const False) (const True) r)
    mapM_ close ss
    return
        Result
            { rLabel = "idle"
            , rOps = cfgConns cfg
            , rOpName = "conn"
            , rElapsedNs = t1 - t0
            , rHist = hist
            , rBefore = before
            , rAfter = after
            , rClientCpuNs = ccpu1 - ccpu0
            , rExtra =
                [ ("connections held", show (cfgConns cfg))
                , ("still alive after hold", show (length (filter id alive)))
                , ("held for (s)", show (cfgDuration cfg))
                , ("server timeout (s)", show (cfgTimeout cfg))
                ,
                    -- Every scheme registers a timer per connection here, so
                    -- this is where a setup burst would show up.
                    ("establish cpu ns per conn", perConn (cfgConns cfg) before early0)
                ,
                    ( "first-window cpu ns per conn"
                    , perConn (cfgConns cfg) early0 early1
                    )
                ,
                    ( "startup cpu ns per conn per s (first window)"
                    , idleCpu (cfgConns cfg) early0 early1 e0 e1
                    )
                ,
                    -- The fairest single number: everything it cost to get the
                    -- connections up and hold them, however it is bucketed.
                    ("total cpu ns per conn", perConn (cfgConns cfg) before holdTo)
                ,
                    ( "idle cpu ns per conn per s (steady)"
                    , idleCpu (cfgConns cfg) holdFrom holdTo hold0 hold1
                    )
                , ("window each (s)", show (cfgDuration cfg))
                ,
                    ( "arrival rate (conn/s)"
                    , if cfgArrivalRate cfg <= 0 then "unpaced" else show (cfgArrivalRate cfg)
                    )
                ]
            }

-- | Server CPU per connection between two samples, unnormalised by time.
perConn :: Int -> Maybe ProcSample -> Maybe ProcSample -> String
perConn conns mb ma = case (mb, ma) of
    (Just b, Just a)
        | conns > 0 ->
            show (fromIntegral (psCpuNs a - psCpuNs b) / fromIntegral conns :: Double)
    _ -> "n/a"

-- | Server CPU spent per held connection per second of doing nothing.
--
-- The steady-state cost of supervising a connection: whatever the timeout
-- mechanism does when there is no traffic at all.
idleCpu :: Int -> Maybe ProcSample -> Maybe ProcSample -> Word64 -> Word64 -> String
idleCpu conns mb ma t0 t1 = case (mb, ma) of
    (Just b, Just a) ->
        let dcpu = fromIntegral (psCpuNs a - psCpuNs b) :: Double
            dt = fromIntegral (t1 - t0) / 1e9 :: Double
         in if dt <= 0 || conns <= 0
                then "n/a"
                else show (dcpu / dt / fromIntegral conns)
    _ -> "n/a"

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = let (a, b) = splitAt n xs in a : chunksOf n b

----------------------------------------------------------------
-- Reading the server's cost out of /proc
--
-- Guarded at runtime rather than with CPP so there is one code path: without
-- /proc this yields Nothing and the report omits CPU and memory, keeping
-- throughput and latency.

data ProcSample = ProcSample
    { psCpuNs :: !Word64
    , psRssKb :: !Int
    , psHwmKb :: !Int
    , psThreads :: !Int
    }

readProc :: Maybe Pid -> IO (Maybe ProcSample)
readProc Nothing = return Nothing
readProc (Just pid) = do
    let d = "/proc/" ++ show pid
    ok <- doesFileExist (d ++ "/stat")
    if not ok
        then return Nothing
        else either (const Nothing) Just <$> try @IOException (sample d)
  where
    sample d = do
        st <- slurp (d ++ "/stat")
        status <- slurp (d ++ "/status")
        tick <- clockTick
        -- The comm field can contain spaces and parentheses, so split after
        -- its closing paren; what follows starts at field 3, making utime
        -- (field 14) index 11 and stime (field 15) index 12.
        let fields = words (drop 1 (dropWhile (/= ')') st))
            at i = if length fields > i then read (fields !! i) else 0 :: Integer
            cpuNs = fromIntegral ((at 11 + at 12) * 1000000000 `div` tick)
        return
            ProcSample
                { psCpuNs = cpuNs
                , psRssKb = field "VmRSS:" status
                , psHwmKb = field "VmHWM:" status
                , psThreads = field "Threads:" status
                }
    slurp p = do
        h <- openFile p ReadMode
        c <- hGetContents h
        length c `seq` hClose h
        return c
    field key status = case filter (key `isPrefixOf`) (lines status) of
        (l : _) -> case words l of
            (_ : v : _) -> read v
            _ -> 0
        [] -> 0

clockTick :: IO Integer
#if defined(mingw32_HOST_OS)
clockTick = return 100
#else
clockTick = fromIntegral <$> getSysVar ClockTick
#endif

----------------------------------------------------------------
-- Reporting

report :: Config -> Int -> Result -> IO ()
report cfg caps r = do
    driverCaps <- getNumCapabilities
    p50 <- histPercentile (rHist r) 0.50
    p99 <- histPercentile (rHist r) 0.99
    p999 <- histPercentile (rHist r) 0.999
    nLat <- histCount (rHist r)
    let elapsedS = fromIntegral (rElapsedNs r) / 1e9 :: Double
        rate = fromIntegral (rOps r) / elapsedS :: Double
        delta f = do
            b <- rBefore r
            a <- rAfter r
            return (f a - f b)
        cpuPerOp =
            (\c -> fromIntegral c / fromIntegral (max 1 (rOps r)) :: Double)
                <$> delta psCpuNs
        clientPerOp =
            fromIntegral (rClientCpuNs r) / fromIntegral (max 1 (rOps r)) :: Double
        ratio = (\sc -> clientPerOp / max 1 sc) <$> cpuPerOp
        stats =
            [ ("scenario", rLabel r)
            , ("connections", show (cfgConns cfg))
            , ("server caps", show caps)
            , ("server rts", cfgServerRts cfg)
            , (rOpName r ++ "s", show (rOps r))
            , ("elapsed s", fmt elapsedS)
            , (rOpName r ++ "/s", fmt rate)
            ]
                ++ opt "server cpu ns per op" (fmt <$> cpuPerOp)
                ++ [ ("client cpu ns per op", fmt clientPerOp)
                   , ("client/server cpu", maybe "n/a" fmt ratio)
                   , ("driver caps", show driverCaps)
                   ]
                ++ opt "server rss delta kb" (show <$> delta psRssKb)
                ++ opt
                    ("server rss kb per " ++ rOpName r)
                    ( if rLabel r == "idle"
                        then
                            fmt
                                . (\d -> fromIntegral d / fromIntegral (max 1 (rOps r)) :: Double)
                                <$> delta psRssKb
                        else Nothing
                    )
                ++ maybe
                    []
                    ( \a ->
                        [ ("server rss kb", show (psRssKb a))
                        , ("server peak rss kb", show (psHwmKb a))
                        , -- OS threads, i.e. capabilities plus the IO manager.
                          -- Warp's per-connection threads are green threads and
                          -- do not appear here; their cost shows up in RSS.
                          ("server os threads", show (psThreads a))
                        ]
                    )
                    (rAfter r)
                ++ ( if nLat == 0
                        then []
                        else
                            [ ("latency p50 ns", show p50)
                            , ("latency p99 ns", show p99)
                            , ("latency p999 ns", show p999)
                            ]
                   )
                ++ rExtra r
    let warnings =
            [ "WARNING: measured window is "
                ++ fmt elapsedS
                ++ "s; CPU is sampled at clock-tick granularity (10ms), so"
                ++ " run for several seconds before trusting cpu/op"
            | elapsedS < 1.0
            ]
                ++ [ "NOTE: the driver used "
                    ++ maybe "?" fmt ratio
                    ++ "x the server's CPU. Loopback benchmarks always have the"
                    ++ " two competing; before trusting a comparison, check that"
                    ++ " throughput has saturated by re-running with more"
                    ++ " --conns. Server cpu/op is inflated at low load because"
                    ++ " idle GHC capabilities spin."
                   | maybe False (> 0.7) ratio
                   ]
    if cfgJson cfg
        then putStrLn $ "{" ++ intercalate "," (map kv stats) ++ "}"
        else do
            forM_ stats $ \(k, v) -> putStrLn (pad k ++ " : " ++ v)
            forM_ warnings $ \w -> hPutStrLn stderr ("\n" ++ w)
  where
    opt k = maybe [] (\v -> [(k, v)])
    fmt x = show (fromIntegral (round (x * 100) :: Integer) / 100 :: Double)
    pad s = s ++ replicate (max 0 (28 - length s)) ' '
    kv (k, v) = show k ++ ":" ++ show v
