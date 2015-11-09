-- Test for command line arguments module.
module Devel.CmdArgsSpec where

import Test.Hspec
import Devel.CmdArgs

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Command line arguments." $ do
    it "We can create values of type CmdArgs" $ do
      let cmdArgs' = CmdArgs "Application.hs" "runDevel" True
      cmdArgs' `shouldBe` (CmdArgs "Application.hs" "runDevel" True :: CmdArgs)
    it "Creates values of type Parser CmdArgs" $ do
      -- How to test that cmdArgs actually creates values of type Parser CmdArgs?
      "to do" `shouldBe` "to do"
