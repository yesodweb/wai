{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Connection supervision for HTTP\/1.1.
--
-- Instead of poking a timer from inside the request and response paths, a
-- connection records /what it is currently doing/ in a 'TVar', and a separate
-- thread decides whether it has been doing it for too long.
--
-- Two things follow from that. The hot path gets cheap: 'tick' is a single STM
-- write, with no clock read and no traffic on the process-wide GHC timer
-- manager. And the timeout policy stops being spread across the server as
-- @tickle@\/@pause@\/@resume@ calls whose purpose can only be reconstructed by
-- tracing them all — it is the 'phaseBudget' table, which is total, so no
-- phase can quietly end up unprotected.
--
-- The connection code calls 'enter' when it moves between phases and 'tick'
-- when it makes progress within one. Nothing else.
module Network.Wai.Handler.Warp.Watchdog (
    -- * What a connection is doing
    Phase (..),
    TaggedPhase (..),
    ConnState,
    newConnState,

    -- * Recording activity
    tick,
    enter,

    -- * The policy
    Budget (..),
    phaseBudget,

    -- * Supervision
    withConnWatchdog,
    watchdog,
) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import qualified Control.Exception as E

#if defined(mingw32_HOST_OS)
import qualified GHC.Event.Windows as EV
#else
import qualified GHC.Event as EV
#endif

----------------------------------------------------------------

-- | What a connection is currently doing.
--
-- These are exactly the distinctions Warp's timeout policy is able to make,
-- because 'phaseBudget' is defined on them and nothing else.
data Phase
    = -- | Waiting for, or reading, a request line and headers. Covers the gap
      -- between keep-alive requests. This is the slowloris case, which is why
      -- 'phaseBudget' gives it a 'Total' budget rather than an idle one.
      ReadingHeaders
    | -- | Reading a request body, after the headers are complete.
      ReadingBody
    | -- | The 'Network.Wai.Application' has control, including the gaps
      -- between fragments of a streaming response body. Deliberately not
      -- subject to a timeout: how long a handler takes is the application's
      -- business, not Warp's.
      RunningApp
    | -- | Writing a response to the socket.
      SendingResponse
    | -- | HTTP\/2 took the connection over and runs its own timers. The
      -- watchdog parks here and never fires.
      Delegated
    deriving (Eq, Show)

-- | A generation counter paired with the current 'Phase'.
--
-- The counter increments on every change, and that is the whole trick: it is
-- all the watchdog needs to tell \"still going\" from \"wedged\", so neither
-- side ever has to read a clock to decide whether progress happened.
data TaggedPhase = TaggedPhase !Int !Phase
    deriving (Eq, Show)

-- | The supervised state of one connection.
--
-- There is no handle type wrapping this — the 'TVar' /is/ the handle, so
-- anything that wants to know what a connection is doing can read it and
-- compose that into its own transaction.
type ConnState = TVar TaggedPhase

newConnState :: IO ConnState
newConnState = newTVarIO $ TaggedPhase 0 ReadingHeaders

----------------------------------------------------------------

-- | Record that the connection made progress in whatever phase it is in.
-- The replacement for @System.TimeManager.tickle@, and the only thing the
-- send and receive loops call.
tick :: ConnState -> IO ()
tick cs = atomically $ modifyTVar' cs $ \(TaggedPhase n p) -> TaggedPhase (n + 1) p

-- | Move the connection into a new phase. Counts as progress too.
--
-- The replacement for @pause@ and @resume@: entering 'RunningApp' is a pause,
-- entering a timed phase is a resume, and unlike the old pair neither one
-- touches the GHC timer manager.
enter :: ConnState -> Phase -> IO ()
enter cs p = atomically $ modifyTVar' cs $ \(TaggedPhase n _) -> TaggedPhase (n + 1) p

----------------------------------------------------------------

-- | How long a connection may spend in a phase, and whether making progress
-- inside that phase buys it more time.
data Budget
    = -- | Not subject to a timeout. May run forever.
      NoLimit
    | -- | Microseconds of /idle/ time. Every 'tick' refreshes it, so the
      -- phase may run indefinitely as long as it keeps moving.
      Extends Int
    | -- | Microseconds for the phase as a whole, from the 'enter' that began
      -- it. Progress does not refresh it; only leaving the phase stops the
      -- clock.
      Total Int
    deriving (Eq, Show)

-- | The entire timeout policy, in one table.
--
-- Every phase has to be given an answer here, which is the point: a phase
-- cannot silently end up unprotected the way a missing @resume@ used to leave
-- a code path unprotected.
--
-- 'Total' for 'ReadingHeaders' is what protects against slowloris, and it
-- replaces @settingsSlowlorisSize@ on this path. The old rule was "a read
-- only counts if it delivered at least 2048 bytes", which bounds a dribbling
-- client's /throughput/ but not the time it can hold a connection: a client
-- sending 2049 bytes per timeout period could stay forever. A total budget
-- bounds the time directly and does not care how the bytes are chunked.
--
-- 'ReadingBody' deliberately stays 'Extends': a large upload is legitimately
-- slow, and capping it outright would break it. A rate-based rule, the way
-- Apache's @RequestReadTimeout body=10,MinRate=1000@ works, is the natural
-- next refinement and would be a change to this function and nothing else.
phaseBudget
    :: Int
    -- ^ Timeout in microseconds.
    -> Phase
    -> Budget
phaseBudget us ReadingHeaders = Total us
phaseBudget us ReadingBody = Extends us
phaseBudget _ RunningApp = NoLimit
phaseBudget us SendingResponse = Extends us
phaseBudget _ Delegated = NoLimit

----------------------------------------------------------------

-- | Supervise an action, abandoning it if the connection stalls.
--
-- Replaces 'System.TimeManager.withHandleKillThread' on the HTTP\/1.1 path,
-- with one difference worth knowing: a stalled connection is cancelled by
-- 'race' rather than by throwing 'System.TimeManager.TimeoutThread' at it, so
-- code that matches on that exception will not see it. The cancellation does
-- not escape: 'race' absorbs the loser's exception and this returns normally.
--
-- With a timeout of zero or less no watchdog runs at all, and 'tick' and
-- 'enter' simply accumulate into a 'TVar' nobody reads.
withConnWatchdog
    :: Int
    -- ^ Timeout in microseconds.
    -> (ConnState -> IO ())
    -> IO ()
withConnWatchdog us action = do
    cs <- newConnState
    if us <= 0
        then
            action cs
        else
            race_ (watchdog us cs) (action cs)

----------------------------------------------------------------

-- | Watch a connection until it stalls, and return the phase it stalled in.
--
-- Returns only on a stall; there is no other exit. Cancelling this thread is
-- how it is stopped, which is what 'race' does when the connection finishes
-- first. It takes no callback either: deciding what a stall means is the
-- caller's job.
--
-- The waiting is done with the GHC timer manager writing to a 'TVar' rather
-- than with @threadDelay@, so an expiring timer arrives as an ordinary STM
-- event and can be waited on /together with/ the connection making a move and
-- with the stop signal. That is what lets a state change cancel an in-flight
-- timer instead of having to outlive it, and what lets teardown be a write
-- rather than an exception.
-- The grace period is /not/ included: 'withConnWatchdog' waits it out with a
-- timer and only starts this once it has elapsed.
watchdog
    :: Int
    -- ^ Timeout in microseconds.
    -> ConnState
    -> IO Phase
watchdog us cs = do
    tmgr <- getTimerManager
    -- Bound in a 'let' so that 'tmgr' is simply in scope below, rather than
    -- being threaded through every helper as an argument.
    let -- Both intervals are a quarter of the timeout, capped. The cap is
        -- what they are in practice; the quarter stops a server configured
        -- with a short timeout inheriting a five second blind spot, and lets
        -- the tests run on millisecond timeouts.
        --
        -- The grace period is why most connections cost nothing: an HTTP/1.1
        -- transaction is usually over well inside it, and 'race_' cancels
        -- this thread when the connection finishes, so the common case never
        -- reaches the loop at all.
        grace = max 1 $ min 5000000 $ us `div` 4 -- 5s
        -- Being woken by a busy connection is the only thing that can happen
        -- repeatedly, so it is the only thing worth throttling. Sleeping this
        -- long after each wake-up bounds the watchdog to one wake-up per
        -- interval however busy the connection is.
        rateLimit = max 1 $ min 1000000 $ us `div` 4 -- 1s

        -- Look at the connection and act on its phase. Deliberately no sleep
        -- first: a watchdog that has only just started has nothing to
        -- rate-limit yet, and making it sleep before arming the real budget
        -- costs two timer registrations per connection and delays the budget
        -- by a rate-limit interval, for no benefit at all.
        loop = do
            tagged0@(TaggedPhase n0 phase) <- readTVarIO cs
            case phaseBudget us phase of
                -- No timeout for this phase, so there is nothing to race.
                -- Block until the connection moves rather than spinning. If
                -- it never moves that is the correct outcome: the
                -- application is allowed to take as long as it likes.
                NoLimit -> do
                    atomically $ do
                        TaggedPhase n _ <- readTVar cs
                        check $ n /= n0
                    threadDelay rateLimit
                    loop
                -- Any progress at all resets the budget.
                Extends t -> attempt phase t (/= tagged0)
                -- Only leaving the phase resets the budget. Progress inside
                -- it is irrelevant, which is what makes this a cap on total
                -- time rather than on idle time.
                Total t -> attempt phase t $ \(TaggedPhase _ p) ->
                    p /= phase

        -- Arm a timer and block on a composed transaction: either it expires
        -- or the connection makes a move that counts. If both landed, treat
        -- it as a move -- a connection that turns out to have been alive
        -- must never be killed. 'withTimer' unregisters on the way out, so
        -- whichever happened first cancels the timer.
        attempt :: Phase -> Int -> (TaggedPhase -> Bool) -> IO Phase
        attempt phase t moved = do
            expired <- newTVarIO False
            stillMoving <-
                withTimer tmgr t (atomically $ writeTVar expired True) $
                    atomically $ do
                        tagged <- readTVar cs
                        e <- readTVar expired
                        check $ e || moved tagged
                        return $ moved tagged
            if stillMoving
                then threadDelay rateLimit >> loop
                else return phase

    threadDelay grace
    loop

----------------------------------------------------------------

-- | Run an action with a one-shot timer armed, cancelling it afterwards.
--
-- Only used where the timer has to be waited on /together with/ the
-- connection state in a single transaction; a plain wait is 'threadDelay',
-- which does the same thing without unregistering a timer that has already
-- fired.
--
-- The timer is registered fresh every time rather than kept and updated:
-- GHC's timeouts are one-shot, and once a key has fired it is gone from the
-- queue, so @updateTimeout@ on it is a silent no-op that would leave the
-- watchdog blocked forever.
withTimer :: TimerManager -> Int -> IO () -> IO a -> IO a
withTimer tmgr us onFire act =
    E.bracket (EV.registerTimeout tmgr us onFire) (EV.unregisterTimeout tmgr) $
        const act

#if defined(mingw32_HOST_OS)
type TimerManager = EV.Manager

getTimerManager :: IO TimerManager
getTimerManager = EV.getSystemManager
#else
type TimerManager = EV.TimerManager

getTimerManager :: IO TimerManager
getTimerManager = EV.getSystemTimerManager
#endif
