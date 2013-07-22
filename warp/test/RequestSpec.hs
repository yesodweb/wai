{-# LANGUAGE OverloadedStrings #-}

module RequestSpec (main, spec) where

import Data.Conduit
import Data.Conduit.List
import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.Types
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "takeHeaders" $ do
    it "takes until blank" $
        blankSafe >>= (`shouldBe` ["foo", "bar", "baz"])
    it "ignored leading whitespace in bodies" $
        whiteSafe >>= (`shouldBe` ["foo", "bar", "baz"])
    it "throws OverLargeHeader when too many" $
        tooMany `shouldThrow` overLargeHeader
    it "throws OverLargeHeader when too large" $
        tooLarge `shouldThrow` overLargeHeader
  where
    blankSafe = (sourceList ["f", "oo\n", "bar\nbaz\n\r\n"]) $$ takeHeaders
    whiteSafe = (sourceList ["foo\r\nbar\r\nbaz\r\n\r\n hi there"]) $$ takeHeaders
    tooMany = (sourceList $ repeat "f\n") $$ takeHeaders
    tooLarge = (sourceList $ repeat "f") $$ takeHeaders

overLargeHeader :: Selector InvalidRequest
overLargeHeader e = e == OverLargeHeader
