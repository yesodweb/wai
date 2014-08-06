module Control.ReaperSpec (spec) where

import Control.Reaper
import Control.Concurrent
import Test.Hspec
import Test.Hspec.QuickCheck
import Data.IORef

type Item = (Int, IORef Int)

spec :: Spec
spec = prop "works" $ \is -> do
    (addItem, wlRef) <- mkReaper defaultReaperSettings
        { reaperAction = mkListAction $ \(i, ref) -> do
            modifyIORef ref succ
            return $ if i > 1
                then Just (pred i, ref)
                else Nothing
        , reaperDelay = 1000
        }

    let mkTest i = do
            ref <- newIORef 0
            let expected = (abs i `mod` 10) + 1
            addItem (expected, ref)
            return (expected, ref)
    tests <- mapM mkTest is

    let test (expected, ref) = do
            actual <- readIORef ref
            actual `shouldBe` (expected :: Int)
    threadDelay 100000
    mapM_ test tests
    NoReaper <- readIORef wlRef
    return ()
