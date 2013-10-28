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
spec = describe "headerLines" $ do
    it "takes until blank" $
        blankSafe >>= (`shouldBe` ["foo", "bar", "baz"])
    it "ignored leading whitespace in bodies" $
        whiteSafe >>= (`shouldBe` ["foo", "bar", "baz"])
    it "throws OverLargeHeader when too many" $
        tooMany `shouldThrow` overLargeHeader
    it "throws OverLargeHeader when too large" $
        tooLarge `shouldThrow` overLargeHeader
  where
    blankSafe = (sourceList ["f", "oo\n", "bar\nbaz\n\r\n"]) $$ headerLines
    whiteSafe = (sourceList ["foo\r\nbar\r\nbaz\r\n\r\n hi there"]) $$ headerLines
    tooMany = (sourceList $ repeat "f\n") $$ headerLines
    tooLarge = (sourceList $ repeat "f") $$ headerLines

overLargeHeader :: Selector InvalidRequest
overLargeHeader e = e == OverLargeHeader
