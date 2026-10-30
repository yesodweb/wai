{-# LANGUAGE NumericUnderscores #-}

module Control.DebounceSpec (main, spec) where

import Control.Concurrent (
    MVar,
    newEmptyMVar,
    newMVar,
    putMVar,
    takeMVar,
    threadDelay,
    tryReadMVar,
 )
import Control.Debounce (
    DebounceSettings (..),
    defaultDebounceSettings,
    leadingEdge,
    leadingMuteEdge,
    trailingDelayEdge,
    trailingEdge,
 )
import qualified Control.Debounce.Internal as DI
import Control.Monad (void)
import Control.Monad.Catch
import Control.Retry (constantDelay, limitRetries, recovering)
import Data.IORef (IORef, modifyIORef, newIORef, readIORef)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import Test.HUnit (assertBool)
import Test.HUnit.Lang (HUnitFailure (HUnitFailure))
import Test.Hspec (Spec, describe, hspec, it, shouldReturn)

spec :: Spec
spec = describe "mkDebounce" $ do
    describe "Leading edge" $ do
        it "works for a single event" $ do
            (ref, debounced, _baton, returnFromWait) <- getDebounce leadingEdge

            debounced
            waitUntil 5 $ readIORef ref `shouldReturn` 1

            returnFromWait
            pause
            readIORef ref `shouldReturn` 1

            -- Try another round
            debounced
            waitUntil 5 $ readIORef ref `shouldReturn` 2

            returnFromWait
            pause
            readIORef ref `shouldReturn` 2

        it "works for multiple events" $ do
            (ref, debounced, baton, returnFromWait) <- getDebounce leadingEdge

            debounced
            waitForBatonToBeTaken baton
            debounced
            pause
            waitUntil 5 $ readIORef ref `shouldReturn` 1

            returnFromWait
            pause
            readIORef ref `shouldReturn` 2
    describe "LeadingMute edge" $ do
        it "works for a single event" $ do
            (ref, debounced, _baton, returnFromWait) <- getDebounce leadingMuteEdge

            debounced
            waitUntil 5 $ readIORef ref `shouldReturn` 1

            returnFromWait
            pause
            readIORef ref `shouldReturn` 1

            -- Try another round
            debounced
            waitUntil 5 $ readIORef ref `shouldReturn` 2

            returnFromWait
            pause
            readIORef ref `shouldReturn` 2

        it "works for multiple events" $ do
            (ref, debounced, baton, returnFromWait) <- getDebounce leadingMuteEdge

            debounced
            waitForBatonToBeTaken baton
            debounced
            pause
            debounced
            waitUntil 5 $ readIORef ref `shouldReturn` 1
            debounced

            returnFromWait
            pause
            readIORef ref `shouldReturn` 1

    describe "Trailing edge" $ do
        it "works for a single event" $ do
            (ref, debounced, _baton, returnFromWait) <- getDebounce trailingEdge

            debounced
            pause
            readIORef ref `shouldReturn` 0

            returnFromWait
            waitUntil 5 $ readIORef ref `shouldReturn` 1

            -- Try another round
            debounced
            pause
            waitUntil 5 $ readIORef ref `shouldReturn` 1

            returnFromWait
            waitUntil 5 $ readIORef ref `shouldReturn` 2

        it "works for multiple events" $ do
            (ref, debounced, baton, returnFromWait) <- getDebounce trailingEdge

            debounced
            waitForBatonToBeTaken baton
            debounced
            pause
            readIORef ref `shouldReturn` 0

            returnFromWait
            waitUntil 5 $ readIORef ref `shouldReturn` 1

    describe "TrailingDelay edge" $ do
        it "works for a single event" $ do
            (ref, debounced, _baton, _returnFromWait) <- getDebounce' True trailingDelayEdge

            debounced
            readIORef ref `shouldReturn` 0

            waitUntil 1 $ readIORef ref `shouldReturn` 1

            -- Try another round
            debounced
            readIORef ref `shouldReturn` 1

            waitUntil 1 $ readIORef ref `shouldReturn` 2

        it "works for multiple events" $ retriggerTrailingDelay 3

-- | Trigger twice, half a cooldown apart, and check that the action fires
-- once, a full cooldown after the second trigger. Only lower bounds on time
-- are asserted; a retrigger which comes too late is retried.
retriggerTrailingDelay :: Int -> IO ()
retriggerTrailingDelay attempts = do
    firedRef <- newIORef []
    baton <- newMVar ()
    debounced <-
        DI.mkDebounceInternal
            baton
            threadDelay
            defaultDebounceSettings
                { debounceFreq = 1_000_000
                , debounceAction = getMonotonicTime >>= \t -> modifyIORef firedRef (t :)
                , debounceEdge = trailingDelayEdge
                }
    start <- getMonotonicTime
    debounced
    threadDelay 500_000
    before2nd <- getMonotonicTime
    debounced
    after2nd <- getMonotonicTime
    if after2nd - start > 0.9
        then
            if attempts > 1
                then retriggerTrailingDelay (attempts - 1)
                else assertBool "The retrigger kept coming after the cooldown" False
        else do
            waitUntil 3 $ length <$> readIORef firedRef `shouldReturn` 1
            [fired] <- readIORef firedRef
            assertBool "Took less than 1 sec after retrigger" $
                fired - before2nd >= 1
            assertBool "Took less than 1.5 sec total" $
                fired - start >= 1.5

-- | Make a controllable delay function
getWaitAction :: IO (p -> IO (), IO ())
getWaitAction = do
    waitVar <- newEmptyMVar
    let waitAction _ = takeMVar waitVar
    let returnFromWait = putMVar waitVar ()
    return (waitAction, returnFromWait)

getDebounce :: DI.DebounceEdge -> IO (IORef Int, IO (), MVar (), IO ())
getDebounce = getDebounce' False

-- | Get a debounce system with access to the internals for testing
getDebounce' :: Bool -> DI.DebounceEdge -> IO (IORef Int, IO (), MVar (), IO ())
getDebounce' useThreadDelay edge = do
    ref <- newIORef 0
    let action = modifyIORef ref (+ 1)

    (waitAction, returnFromWait) <-
        if useThreadDelay
            then pure (threadDelay, pure ())
            else getWaitAction

    baton <- newMVar ()

    debounced <-
        DI.mkDebounceInternal
            baton
            waitAction
            defaultDebounceSettings
                { debounceFreq = 1_000_000 -- !!! used in 'TrailingDelay' test
                , debounceAction = action
                , debounceEdge = edge
                }

    return (ref, debounced, baton, returnFromWait)

-- | Pause briefly (100ms)
pause :: IO ()
pause = threadDelay 100_000

waitForBatonToBeTaken :: MVar () -> IO ()
waitForBatonToBeTaken baton =
    waitUntil 5 $ tryReadMVar baton `shouldReturn` Nothing

-- | Wait up to n seconds for an action to complete without throwing an HUnitFailure
waitUntil :: Int -> IO a -> IO ()
waitUntil n action =
    recovering policy [handler] (\_status -> void action)
  where
    policy = constantDelay 1000 `mappend` limitRetries (n * 1000) -- 1ms * n * 1000 tries = n seconds
    handler _status = Handler (\HUnitFailure{} -> return True)

main :: IO ()
main = hspec spec
