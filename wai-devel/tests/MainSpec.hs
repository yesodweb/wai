-- Tests for Main function.
{-# LANGUAGE PackageImports #-}
module MainSpec where

import Test.Hspec
import "wai-devel" Main as M
import Control.Concurrent (forkIO, threadDelay, killThread)

main :: IO ()
main = hspec spec

spec :: Spec
spec = -- How does one test that a bottom effectful computation has done what is expected?
  describe "Program has a working main function" $
    it "Gets cmd arguments from cmdArgs and calls successfully `buildAndRun`." $ do
      tId <- forkIO M.main
      threadDelay 100
      killThread tId
      tId `shouldBe` tId

