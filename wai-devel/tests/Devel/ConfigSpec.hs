module Devel.ConfigSpec where

import Test.Hspec
import Devel.Config

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Able to extract PATH and package database from `stack path` output" $ do
    it "Able to extract bin-path (PATH) when there is a bin-path in the key part of the association list" $ 
      -- wut?    
      getPath ("bin-path", "bin-path: ~/bin/path/found") `shouldBe` "bin-path:~/bin/path/found"

    it "Gives an empty string when bin-path isn't in the association list" $
      getPath ("not-bin-path", "ghc-paths: ~/not/bin/path") `shouldBe` ""
      
    it "Able to extract ghc-package-path (GHC_PACKAGE_PATH) whe it's part of the association list" $ 
      -- wut?
      getPkgDb ("ghc-package-path", "ghc-package-path: ~/pkg/db/found") `shouldBe` "ghc-package-path:~/pkg/db/found"

    it "Gives an empty string when snapshot-pkg-db isn't in the association list" $
      getPkgDb ("not-snapshot-pkg-db", "ghc-paths: ~/not/pkg/db") `shouldBe` ""


  describe "setConfig doesn't throw an exception." $
    it "setConfig gives back a unit ()." $ do
      value <- setConfig
      value `shouldBe` ()
