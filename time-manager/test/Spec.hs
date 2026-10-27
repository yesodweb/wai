{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Test.Hspec

#if defined(mingw32_HOST_OS)
import qualified System.TimeManager.Windows as EV
#else
import qualified GHC.Event as EV
#endif

import Control.Concurrent (
    MVar,
    forkIO,
    newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
    tryPutMVar,
 )
import Control.Concurrent.STM (atomically)
import Control.Exception (AssertionFailed (..), finally, throwIO)
import Control.Monad (forM_, void)
import Data.IORef as I (
    IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
    writeIORef,
 )
import GHC.Clock (getMonotonicTimeNSec)
import GHC.Conc.Sync (
    getUncaughtExceptionHandler,
    setUncaughtExceptionHandler,
 )
import System.ThreadManager (
    ThreadManager,
    countManaged,
    forkManaged,
    forkManagedFinally,
    forkManagedTimeout,
    forkManagedTimeoutFinally,
    forkManagedUnmaskFinally,
    newThreadManager,
    reserveManaged,
    stopAfter,
    takeReservation,
    waitUntilAllGone,
 )
import System.TimeManager
import System.TimeManager.Internal
import qualified System.Timeout as Timeout
import Test.HUnit (assertBool)

main :: IO ()
main = hspec $ do
    describe "ThreadManager, once stopped" $ do
        -- 'setup' refuses to record a thread forked before the stop and
        -- scheduled after it, by throwing.  That must not escape the fork,
        -- and must not take the cleanup with it.
        it "does not let forkManagedTimeout's refusal escape the fork" $ do
            tm <- newThreadManager
            stopAfter tm (return ()) $ \_ -> return ()
            ran <- freshRef
            escaped <- freshRef
            before <- getUncaughtExceptionHandler
            setUncaughtExceptionHandler $ \_ -> I.writeIORef escaped True
            forkManagedTimeout tm defaultManager "probe" $ \_ -> I.writeIORef ran True
            threadDelay 200_000
            setUncaughtExceptionHandler before
            ran `refShouldBe` False
            escaped `refShouldBe` False

        itRunsTheCleanupOf "forkManagedFinally" $
            \tm label -> forkManagedFinally tm label (return ())

        itRunsTheCleanupOf "forkManagedTimeoutFinally" $
            \tm label -> forkManagedTimeoutFinally tm defaultManager label (\_ -> return ())

        itRunsTheCleanupOf "forkManagedUnmaskFinally" $
            \tm label -> forkManagedUnmaskFinally tm label (\_ -> return ())

    describe "ThreadManager, the action of a fork with a cleanup" $ do
        itLetsTheExceptionOutOf "forkManagedFinally" forkManagedFinally

        itLetsTheExceptionOutOf "forkManagedTimeoutFinally" $
            \tm label io -> forkManagedTimeoutFinally tm defaultManager label (\_ -> io)

        itLetsTheExceptionOutOf "forkManagedUnmaskFinally" $
            \tm label io -> forkManagedUnmaskFinally tm label (\unmask -> unmask io)

    describe "ThreadManager" $ do
        it "fails loudly when a reservation is taken by another manager" $ do
            a <- newThreadManager
            b <- newThreadManager
            rsv <- reserveManaged a
            takeReservation b rsv (return ())
            atomically (countManaged b)
                `shouldThrow` (\(AssertionFailed _) -> True)

        it "has none left only once their cleanups are done" $ do
            tm <- newThreadManager
            ref <- freshRef
            forkManagedFinally
                tm
                "probe"
                (threadDelay 100_000)
                (threadDelay 300_000 >> I.writeIORef ref True)
            -- The action is over and the cleanup is still running.
            threadDelay 200_000
            waitUntilAllGone tm
            ref `refShouldBe` True

        it "still owns a thread it killed until that thread is gone" $ do
            tm <- newThreadManager
            ref <- freshRef
            started <- newEmptyMVar
            -- The cleanup here is the thread's own, not one the manager
            -- runs for it, so what is tested is an ordinary managed
            -- thread unwinding.
            forkManaged tm "probe" $
                (putMVar started () >> threadDelay 10_000_000)
                    `finally` (threadDelay 300_000 >> I.writeIORef ref True)
            takeMVar started
            threadDelay 50_000
            -- 'throwTo' waits for the exception to be taken, not for the
            -- thread to be gone.
            stopAfter tm (return ()) $ \_ -> return ()
            threadDelay 50_000
            atomically (countManaged tm) `shouldReturn` 1
            waitUntilAllGone tm
            ref `refShouldBe` True

    describe "TimeManager" $ do
        it "defaultManager == no manager" $
            defaultManager `shouldSatisfy` isNoManager

        it "initializes negative manager" $ do
            let check = (`shouldBe` defaultManager)
            initialize (-10) >>= check
            withManager (-5) check

        it "empty handle is correct" $
            handleTimeout emptyHandle `shouldBe` 0

        it "empty handle check is consistent" $ do
            assertBool "emptyHandle not empty" $
                isEmptyHandle emptyHandle

        it "gives emptyHandle when registering defaultManager" $ do
            hndl <- register defaultManager $ pure ()
            assertBool "got non-empty handle" $ isEmptyHandle hndl

        it "throws TimeoutThread exception" $
            throwsTimeoutThread $ do
                mngr <- initialize timeoutAmount
                _hndl <- registerKillThread mngr $ pure ()
                waitLong

        it "defaultManager doesn't kill thread" $ do
            _hndl <- registerKillThread defaultManager $ pure ()
            waitShort

        it "withHandle: registers timeout" $
            withHandleTest mgr1 $ \check _ -> do
                waitShort
                check True

        it "withHandle: doesn't register timeout" $
            withHandleTest defaultManager $ \check _ -> do
                waitShort
                check False

        -- We make a ref on the outside, to check that the ref is indeed
        -- set before the timeout kills the action inside.
        it "withHandleKillThread: registers timeout (and kills)" $ do
            ref <- freshRef
            withHandleKillTest (Just ref) mgr1 $ \_ _ ->
                throwsTimeoutThread waitShort
            ref `refShouldBe` True

        it "withHandleKillThread: doesn't register timeout" $
            withHandleKillTest Nothing defaultManager $ \check _ -> do
                waitShort >> check False

        it "cancel/pause works as expected" $ do
            m <- mkTestManager
            let killUnless f = do
                    hndl <- registerKillThread m (pure ())
                    _ <- f hndl
                    waitLong
            throwsTimeoutThread $ killUnless pure
            killUnless cancel
            killUnless pause

        it "tickle works as expected" $ do
            m <- mkTestManager
            withHandleTest m $ \check hndl -> do
                forM_ [(1 :: Int) .. 20] $ \_ -> do
                    waitShort
                    tickle hndl
                check False

        let runAndWaitForTimeout f =
                runIt $ \hndl -> do
                    void $ f hndl
                    waitLong
        it "resume works as expected (nothing)" $ do
            -- Doing nothing kills the thread
            throwsTimeoutThread . runAndWaitForTimeout $ \_ -> pure ()
        it "resume works as expected (pause)" $ do
            -- Pausing stops the kill
            runAndWaitForTimeout $ \hndl -> do
                waitShort >> pause hndl
        it "resume works as expected (pause/resume)" $ do
            -- Resuming kills the thread again
            throwsTimeoutThread . runAndWaitForTimeout $ \hndl -> do
                waitShort >> pause hndl
                waitLong >> resume hndl
        it "resume works as expected (cancel/resume)" $ do
            -- Cancelling is unresumable
            runAndWaitForTimeout $ \hndl -> do
                waitShort >> cancel hndl
                waitLong >> resume hndl
        it "resume works as expected (cancel/pause/resume)" $ do
            -- Cancelling and then pausing is still unresumable
            runAndWaitForTimeout $ \hndl -> do
                waitShort >> cancel hndl
                waitShort >> pause hndl
                waitLong >> resume hndl
            -- Pausing, then cancelling doesn't change anything
            runAndWaitForTimeout $ \hndl -> do
                waitShort >> pause hndl
                waitShort >> cancel hndl
                waitLong >> resume hndl
        it "finished timeout won't resume" $ do
            -- If the timeout action runs, resume shouldn't work
            counter <- I.newIORef (0 :: Int)
            m <- mkTestManager
            let increase = I.atomicModifyIORef' counter $ \i -> (i + 1, ())
            withHandle m increase $ \h -> do
                let checkCount x = do
                        i <- I.readIORef counter
                        i `shouldBe` x
                    timeoutOnlyRanOnce = waitLong >> checkCount 1

                checkCount 0
                -- waiting lets the timeout
                timeoutOnlyRanOnce
                -- resuming should not influence the counter
                resume h
                timeoutOnlyRanOnce
                -- pausing after it runs also doesn't re-arm the timeout
                pause h
                resume h
                timeoutOnlyRanOnce
                -- cancel also doesn't re-arm the timeout
                cancel h
                pause h
                resume h
                timeoutOnlyRanOnce

        it "reschedules an expired callback for the remaining timeout" $ do
            m <- initialize 1_000_000
            fired <- newEmptyMVar
            withHandle m (putMVar fired ()) $ \h -> do
                threadDelay 500_000
                -- Model a renewal after GHC has queued the original callback.
                -- Updating GHC's timer here would remove that stale callback.
                withLock (handleLock h) $ do
                    now <- getMonotonicTimeNSec
                    I.writeIORef (handleLastRenewed h) now
                Timeout.timeout 750_000 (takeMVar fired)
                    `shouldReturn` Nothing
                Timeout.timeout 1_000_000 (takeMVar fired)
                    `shouldReturn` Just ()

        it "resume also works as tickle" $
            testResume resume

        it "resume also works as tickle with pauses" $
            testResume $ \hndl -> do
                resume hndl
                pause hndl
                resume hndl

        it "old resume did NOT work as tickle" $
            throwsTimeoutThread $
                testResume oldResume
  where
    withHandleTest = withTest withHandle Nothing
    withHandleKillTest = withTest withHandleKillThread
    -- Test that starts with a 'False' IORef and on timeout sets it to true
    withTest withF mRef m f = do
        ref <- maybe freshRef pure mRef
        withF m (I.writeIORef ref True) . f $ refShouldBe ref
    -- run with a 20ms timeout and kill
    runIt f = do
        m <- mkTestManager
        void $ f =<< registerKillThread m (pure ())
    timeoutAmount = 20_000
    mkTestManager = initialize timeoutAmount
    -- Waiting a lot less than the timeout takes
    waitShort = threadDelay $ timeoutAmount `div` 5
    -- Waiting a lot longer than the timeout takes
    waitLong = threadDelay $ timeoutAmount * 5
    -- "resuming" every 2.5ms 20 times
    testResume f = do
        runIt $ \hndl -> do
            forM_ [(1 :: Int) .. 20] $ \_ -> waitShort >> f hndl

mgr1 :: Manager
mgr1 = Manager 1

-- | The cleanup of a fork that takes one runs even when the manager has
--   been stopped and the thread is refused.
itRunsTheCleanupOf
    :: String
    -> (ThreadManager -> String -> IO () -> IO ())
    -- ^ The fork, with its action already given and its cleanup to come
    -> Spec
itRunsTheCleanupOf name fork =
    it ("runs the cleanup of " ++ name) $ do
        tm <- newThreadManager
        stopAfter tm (return ()) $ \_ -> return ()
        done <- newEmptyMVar
        fork tm "probe" $ putMVar done ()
        ranWithin 500_000 done `shouldReturn` True

-- | An exception from the action of a fork that takes a cleanup leaves the
--   thread, after the cleanup has run.
itLetsTheExceptionOutOf
    :: String
    -> (ThreadManager -> String -> IO () -> IO () -> IO ())
    -> Spec
itLetsTheExceptionOutOf name fork =
    it ("lets an exception from the action of " ++ name ++ " out") $ do
        tm <- newThreadManager
        escaped <- newEmptyMVar
        cleaned <- freshRef
        before <- getUncaughtExceptionHandler
        setUncaughtExceptionHandler $ \_ -> void $ tryPutMVar escaped ()
        fork tm "probe" (throwIO $ userError "probe") (I.writeIORef cleaned True)
        out <- ranWithin 500_000 escaped
        setUncaughtExceptionHandler before
        out `shouldBe` True
        cleaned `refShouldBe` True

-- | Whether the 'MVar' was filled within the time.
ranWithin :: Int -> MVar () -> IO Bool
ranWithin us var = do
    r <- newEmptyMVar
    void $ forkIO $ takeMVar var >> putMVar r True
    void $ forkIO $ threadDelay us >> putMVar r False
    takeMVar r

freshRef :: IO (IORef Bool)
freshRef = I.newIORef False

refShouldBe :: IORef Bool -> Bool -> IO ()
refShouldBe ref expected =
    I.readIORef ref >>= (`shouldBe` expected)

throwsTimeoutThread :: IO () -> Expectation
throwsTimeoutThread t = t `shouldThrow` (const True :: TimeoutThread -> Bool)

deriving instance Eq Manager
deriving instance Show Manager

-- copied from time-manager-0.3.0 to check it actually is broken
oldResume :: Handle -> IO ()
oldResume h | isEmptyHandle h = return ()
oldResume Handle{..} = do
    key <- EV.registerTimeout handleTimerManager handleTimeout handleAction
    I.writeIORef handleState $ Active key
