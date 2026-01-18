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
main = hspec $
    describe "TimeManager" $ do
        it "empty check is correct" $
            handleTimeout emptyHandle `shouldBe` 0

        it "empty handle check is consistent" $ do
            assertBool "emptyHandle not empty" $
                isEmptyHandle emptyHandle

        it "defaultManager == no manager" $
            defaultManager `shouldSatisfy` isNoManager

        it "initializes negative manager" $ do
            let check = (`shouldBe` defaultManager)
            initialize (-10) >>= check
            withManager (-5) check

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

        let withHandleTest m expected = do
                ref <- newIORef False
                withHandle m (writeIORef ref True) $ \_hndl -> do
                    threadDelay 2000
                    b <- readIORef ref
                    b `shouldBe` expected

        it "withHandle: registers timeout" $
            withHandleTest mgr1 True

        it "withHandle: doesn't register timeout" $
            withHandleTest defaultManager False

        let withHandleKillTest m f = do
                ref <- newIORef False
                withHandleKillThread m (writeIORef ref True) $
                    \_hndl -> f $ do
                        threadDelay 2000
                        b <- readIORef ref
                        b `shouldBe` False

        it "withHandleKillThread: registers timeout" $
            withHandleKillTest mgr1 throwsTimeoutThread

        it "withHandleKillThread: doesn't register timeout" $
            withHandleKillTest defaultManager id

        it "cancel/pause works as expected" $ do
            m <- initialize 100
            let runIt f = do
                    hndl <- registerKillThread m (pure ())
                    _ <- f hndl
                    threadDelay 1000
            throwsTimeoutThread $ runIt pure
            runIt cancel
            runIt pause

-- tickle
-- resume

mgr1 :: Manager
mgr1 = Manager 1

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
