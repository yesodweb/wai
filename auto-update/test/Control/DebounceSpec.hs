module Control.DebounceSpec (main, spec) where

import Control.Concurrent
import Control.Debounce
import qualified Control.Debounce.Internal as DI
import Control.Monad
import Control.Monad.Catch
import Control.Retry
import Data.IORef
import Test.HUnit.Lang
import Test.Hspec

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

    describe "Trailing edge" $ do
        it "works for a single event" $ do
            (ref, debounced, _baton, returnFromWait) <- getDebounce trailingEdge

            debounced
            pause
            waitUntil 5 $ readIORef ref `shouldReturn` 0

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
            waitUntil 5 $ readIORef ref `shouldReturn` 0

            returnFromWait
            waitUntil 5 $ readIORef ref `shouldReturn` 1


-- | Make a controllable delay function
getWaitAction :: IO (p -> IO (), IO ())
getWaitAction = do
    waitVar <- newEmptyMVar
    let waitAction _ = takeMVar waitVar
    let returnFromWait = putMVar waitVar ()
    return (waitAction, returnFromWait)

-- | Get a debounce system with access to the internals for testing
getDebounce :: DI.DebounceEdge -> IO (IORef Int, IO (), MVar (), IO ())
getDebounce edge = do
  ref <- newIORef 0
  let action = modifyIORef ref (+ 1)

  (waitAction, returnFromWait) <- getWaitAction

  baton <- newEmptyMVar

  debounced <- DI.mkDebounceInternal baton waitAction defaultDebounceSettings {
    debounceFreq = 5000000 -- unused
    , debounceAction = action
    , debounceEdge = edge
    }

  return (ref, debounced, baton, returnFromWait)

-- | Pause briefly (100ms)
pause :: IO ()
pause = threadDelay 100000

waitForBatonToBeTaken :: MVar () -> IO ()
waitForBatonToBeTaken baton = waitUntil 5 $ tryReadMVar baton `shouldReturn` Nothing

-- | Wait up to n seconds for an action to complete without throwing an HUnitFailure
waitUntil :: Int -> IO a -> IO ()
waitUntil n action = recovering policy [handler] (\_status -> void action)
  where policy = constantDelay 1000 `mappend` limitRetries (n * 1000) -- 1ms * n * 1000 tries = n seconds
        handler _status = Handler (\HUnitFailure{} -> return True)

main :: IO ()
main = hspec spec
