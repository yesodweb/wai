{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, void)
import Data.IORef as I (IORef, newIORef, readIORef, writeIORef)
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
                mngr <- initialize 1
                _hndl <- registerKillThread mngr $ pure ()
                threadDelay 100

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
                throwsTimeoutThread $ threadDelay 100
            ref `refShouldBe` True

        it "withHandleKillThread: doesn't register timeout" $
            withHandleKillTest Nothing defaultManager $ \check _ -> do
                threadDelay 200
                check False

        it "cancel/pause works as expected" $ do
            m <- initialize 100
            let runIt f = do
                    hndl <- registerKillThread m (pure ())
                    _ <- f hndl
                    threadDelay 1000
            throwsTimeoutThread $ runIt pure
            runIt cancel
            runIt pause

        it "tickle works as expected" $ do
            m <- initialize 10_000
            withHandleTest m $ \check hndl -> do
                forM_ [(1 :: Int) .. 20] $ \_ -> do
                    threadDelay 1000
                    tickle hndl
                check False

        let runIt f = do
                m <- initialize 10_000
                void $ f =<< registerKillThread m (pure ())
        it "resume works as expected" $ do
            let runAndWaitForTimeout f =
                    runIt $ \hndl -> do
                        void $ f hndl
                        threadDelay 20_000
            -- Doing nothing kills the thread
            throwsTimeoutThread . runAndWaitForTimeout $ \_ -> pure ()
            -- Pausing stops the kill
            runAndWaitForTimeout $ \hndl -> do
                threadDelay 2500
                pause hndl
            -- Resuming kills the thread again
            throwsTimeoutThread . runAndWaitForTimeout $ \hndl -> do
                threadDelay 2500
                pause hndl
                threadDelay 20_000
                resume hndl

        -- "resuming" every 2.5ms 20 times
        let testResume f = do
                runIt $ \hndl -> do
                    forM_ [(1 :: Int) .. 20] $ \_ -> do
                        threadDelay 2500
                        f hndl
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
        withF m (writeIORef ref True) . f $ refShouldBe ref

mgr1 :: Manager
mgr1 = Manager 1

freshRef :: IO (IORef Bool)
freshRef = newIORef False

refShouldBe :: IORef Bool -> Bool -> IO ()
refShouldBe ref expected =
    readIORef ref >>= (`shouldBe` expected)

throwsTimeoutThread :: IO () -> Expectation
throwsTimeoutThread t = t `shouldThrow` (const True :: TimeoutThread -> Bool)

deriving instance Eq Manager
deriving instance Show Manager

-- copied from time-manager-0.3.0 to check it actually is broken
oldResume :: Handle -> IO ()
oldResume h | isEmptyHandle h = return ()
oldResume Handle{..} = do
    mgr <- getTimerManager
    key <- EV.registerTimeout mgr handleTimeout handleAction
    I.writeIORef handleKeyRef key
