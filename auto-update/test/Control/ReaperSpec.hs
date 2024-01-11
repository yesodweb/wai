module Control.ReaperSpec (spec) where

-- import Control.Concurrent
-- import Control.Reaper
-- import Data.IORef
import Test.Hspec

-- import Test.Hspec.QuickCheck

spec :: Spec
spec = return ()

--   prop "works" $ \is -> do
--     reaper <- mkReaper defaultReaperSettings
--         { reaperAction = action
--         , reaperDelay = 1000
--         }

--     let mkTestCase i = do
--             ref <- newIORef 0
--             let expected = (abs i `mod` 10) + 1
--             reaperAdd reaper (expected, ref)
--             return (expected, ref)
--     testCases <- mapM mkTestCase is

--     let test (expected, ref) = do
--             actual <- readIORef ref
--             actual `shouldBe` (expected :: Int)
--     threadDelay 100000
--     mapM_ test testCases
--     [] <- reaperRead reaper
--     return ()

-- type Item = (Int, IORef Int)

-- action = mkListAction $ \(i, ref) -> do
--     modifyIORef ref succ
--     return $ if i > 1
--              then Just (pred i, ref)
--              else Nothing
