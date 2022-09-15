module Control.AutoUpdateSpec (spec) where

-- import Control.AutoUpdate
-- import Control.Concurrent (threadDelay)
-- import Control.Monad (replicateM_, forM_)
-- import Data.IORef
import Test.Hspec
-- import Test.Hspec.QuickCheck

spec :: Spec
spec = return ()
  -- do
  --   prop "incrementer" $ \st' -> do
  --       let st = abs st' `mod` 10000
  --       ref <- newIORef 0
  --       next <- mkAutoUpdate defaultUpdateSettings
  --           { updateAction = atomicModifyIORef ref $ \i ->
  --               let i' = succ i in i' `seq` (i', i')
  --           , updateSpawnThreshold = st
  --           , updateFreq = 10000
  --           }

  --       forM_ [1..st + 1] $ \i -> do
  --           j <- next
  --           j `shouldBe` i

  --       replicateM_ 50 $ do
  --           i <- next
  --           i `shouldBe` st + 2

  --       threadDelay 60000
  --       last1 <- readIORef ref
  --       threadDelay 20000
  --       last2 <- readIORef ref
  --       last2 `shouldBe` last1
