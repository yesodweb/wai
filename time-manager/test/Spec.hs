{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, void)
import Data.IORef as I (
    IORef,
    atomicModifyIORef',
    newIORef,
    readIORef,
    writeIORef,
 )
import System.TimeManager
import System.TimeManager.Internal
import Test.HUnit (assertBool)
import Test.Hspec

#if defined(mingw32_HOST_OS)
import qualified GHC.Event.Windows as EV
#else
import qualified GHC.Event as EV
#endif

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
                threadDelay $ timeoutAmount * 2

        it "defaultManager doesn't kill thread" $ do
            _hndl <- registerKillThread defaultManager $ pure ()
            threadDelay 2000

        it "withHandle: registers timeout" $
            withHandleTest mgr1 $ \check _ -> do
                threadDelay 2000
                check True

        it "withHandle: doesn't register timeout" $
            withHandleTest defaultManager $ \check _ -> do
                threadDelay 2000
                check False

        -- We make a ref on the outside, to check that the ref is indeed
        -- set before the timeout kills the action inside.
        it "withHandleKillThread: registers timeout (and kills)" $ do
            ref <- freshRef
            withHandleKillTest (Just ref) mgr1 $ \_ _ ->
                throwsTimeoutThread $ threadDelay 1000
            ref `refShouldBe` True

        it "withHandleKillThread: doesn't register timeout" $
            withHandleKillTest Nothing defaultManager $ \check _ -> do
                threadDelay 200
                check False

        it "cancel/pause works as expected" $ do
            m <- initialize timeoutAmount
            let killUnless f = do
                    hndl <- registerKillThread m (pure ())
                    _ <- f hndl
                    threadDelay $ timeoutAmount * 2
            throwsTimeoutThread $ killUnless pure
            killUnless cancel
            killUnless pause

        it "tickle works as expected" $ do
            m <- initialize timeoutAmount
            withHandleTest m $ \check hndl -> do
                forM_ [(1 :: Int) .. 20] $ \_ -> do
                    threadDelay $ timeoutAmount `div` 10
                    tickle hndl
                check False

        let runAndWaitForTimeout f =
                runIt $ \hndl -> do
                    void $ f hndl
                    threadDelay $ timeoutAmount * 5
        it "resume works as expected (nothing)" $ do
            -- Doing nothing kills the thread
            throwsTimeoutThread . runAndWaitForTimeout $ \_ -> pure ()
        it "resume works as expected (pause)" $ do
            -- Pausing stops the kill
            runAndWaitForTimeout $ \hndl -> do
                threadDelay $ timeoutAmount `div` 4
                pause hndl
        it "resume works as expected (pause/resume)" $ do
            -- Resuming kills the thread again
            throwsTimeoutThread . runAndWaitForTimeout $ \hndl -> do
                threadDelay $ timeoutAmount `div` 4
                pause hndl
                threadDelay $ timeoutAmount * 5
                resume hndl
        it "resume works as expected (cancel/resume)" $ do
            -- Cancelling is unresumable
            runAndWaitForTimeout $ \hndl -> do
                threadDelay $ timeoutAmount `div` 4
                cancel hndl
                threadDelay $ timeoutAmount * 5
                resume hndl
        it "resume works as expected (cancel/pause/resume)" $ do
            -- Cancelling and then pausing is still unresumable
            runAndWaitForTimeout $ \hndl -> do
                threadDelay $ timeoutAmount `div` 4
                cancel hndl
                threadDelay $ timeoutAmount `div` 4
                pause hndl
                threadDelay $ timeoutAmount * 5
                resume hndl
            -- Pausing, then cancelling doesn't change anything
            runAndWaitForTimeout $ \hndl -> do
                threadDelay $ timeoutAmount `div` 4
                pause hndl
                threadDelay $ timeoutAmount `div` 4
                cancel hndl
                threadDelay $ timeoutAmount * 5
                resume hndl
        it "finished timeout won't resume" $ do
            -- If the timeout action runs, resume shouldn't work
            counter <- I.newIORef (0 :: Int)
            m <- initialize timeoutAmount
            let increase = I.atomicModifyIORef' counter $ \i -> (i + 1, ())
            withHandle m increase $ \h -> do
                let checkCount x = do
                        i <- I.readIORef counter
                        i `shouldBe` x
                    timeoutOnlyRanOnce = do
                        threadDelay $ timeoutAmount * 2
                        checkCount 1

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
  where
    timeoutAmount = 10_000
    withHandleTest = withTest withHandle Nothing
    withHandleKillTest = withTest withHandleKillThread
    -- Test that starts with a 'False' IORef and on timeout sets it to true
    withTest withF mRef m f = do
        ref <- maybe freshRef pure mRef
        withF m (I.writeIORef ref True) . f $ refShouldBe ref
    -- run with a 10ms timeout and kill
    runIt f = do
        m <- initialize timeoutAmount
        void $ f =<< registerKillThread m (pure ())
    -- "resuming" every 2.5ms 20 times
    testResume f = do
        runIt $ \hndl -> do
            forM_ [(1 :: Int) .. 20] $ \_ -> do
                threadDelay $ timeoutAmount `div` 4
                f hndl

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
