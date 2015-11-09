-- Tests for Devel.hs
module DevelSpec where

import Test.Hspec
import Devel
import Control.Concurrent

main :: IO ()
main = hspec spec

spec :: Spec
spec = -- How does one test that a bottom effectful computation has done what is expected?
  describe "buildAndRun is able to successfully call build." $ do
    it "Able to apply configs from setConfig" $
      pendingWith "Is it even possible to test for this?"
    it "buildAndRun is able to pick up the value of PORT" $
      pendingWith "Is it even possible to test for this?"
    it "Successfully calls build. Does not throw an exception." $ do
      tId <- forkIO $ buildAndRun "Application.hs" "develMain" True
      threadDelay 100
      killThread tId
      tId `shouldBe` tId

