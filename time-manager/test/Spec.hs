{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Test.Hspec

#if defined(mingw32_HOST_OS)
-- -- Uncomment when reenabling the tests for Windows
-- import qualified GHC.Event.Windows as EV
#else
import qualified GHC.Event as EV
#endif

#if defined(mingw32_HOST_OS)
main :: IO ()
main = hspec $ do
    describe "TimeManager" $ do
        it "tests don't work on windows" $
            pendingWith "requires more testing on a Windows machine"
#else
import Control.Concurrent (
    forkIO,
    newEmptyMVar,
    putMVar,
    takeMVar,
    threadDelay,
    yield,
 )
import qualified Control.Exception as E
import Control.Monad (forM_, unless, void)
import Data.IORef as I (
    IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
    writeIORef,
 )
import GHC.Clock (getMonotonicTimeNSec)
import System.ThreadManager (forkManagedTimeout, newThreadManager)
import System.TimeManager
import System.TimeManager.Internal
import Test.HUnit (assertBool)

main :: IO ()
main = hspec $ do
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

        -- The body ends just as the timeout expires, so that the kill is
        -- decided on while the body is leaving.  It must not land after
        -- 'withHandleKillThread' has returned.
        it "withHandleKillThread: TimeoutThread does not escape" $ do
            m <- initialize 2_000
            escaped <- I.newIORef (0 :: Int)
            forM_ [1 .. 1_000 :: Int] $ \i -> do
                r <- E.try $ do
                    withHandleKillThread m (pure ()) $ \_ ->
                        threadDelay $ 1_900 + i `mod` 200
                    threadDelay 1_000
                case r of
                    Left TimeoutThread -> I.atomicModifyIORef' escaped $ \n -> (n + 1, ())
                    Right () -> pure ()
            I.readIORef escaped `shouldReturn` 0

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

    describe "ThreadManager" $ do
        -- A thread timed out while it cannot take the kill must not hold up
        -- the system timer manager, which every 'threadDelay' waits on.
        it "forkManagedTimeout: a masked thread does not stop the timers" $ do
            tm <- initialize 10_000 >>= newThreadManager
            release <- newEmptyMVar
            forkManagedTimeout tm "masked" $ \_ ->
                E.uninterruptibleMask_ $ takeMVar release
            -- Released without the timer manager, which may be stuck.
            t0 <- getMonotonicTimeNSec
            _ <- forkIO $ do
                let spin = do
                        now <- getMonotonicTimeNSec
                        unless (now - t0 >= 1_000_000_000) $ yield >> spin
                spin
                putMVar release ()
            threadDelay 100_000
            t1 <- getMonotonicTimeNSec
            (t1 - t0) `shouldSatisfy` (< 500_000_000)
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
#endif
