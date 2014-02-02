{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module RequestSpec (main, spec) where

import Data.Conduit
import Data.Conduit.List
import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.RequestHeader (parseByteRanges)
import Network.Wai.Handler.Warp.Types
import Test.Hspec
import qualified Data.ByteString.Char8 as S8
import qualified Network.HTTP.Types.Header as HH

deriving instance Show HH.ByteRange
deriving instance Eq HH.ByteRange

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "headerLines" $ do
    it "takes until blank" $
        blankSafe >>= (`shouldBe` ["foo", "bar", "baz"])
    it "ignored leading whitespace in bodies" $
        whiteSafe >>= (`shouldBe` ["foo", "bar", "baz"])
    it "throws OverLargeHeader when too many" $
        tooMany `shouldThrow` overLargeHeader
    it "throws OverLargeHeader when too large" $
        tooLarge `shouldThrow` overLargeHeader
  describe "parseByteRanges" $ do
    let test x y = it x $ parseByteRanges (S8.pack x) `shouldBe` y
    test "bytes=0-499" $ Just [HH.ByteRangeFromTo 0 499]
    test "bytes=500-999" $ Just [HH.ByteRangeFromTo 500 999]
    test "bytes=-500" $ Just [HH.ByteRangeSuffix 500]
    test "bytes=9500-" $ Just [HH.ByteRangeFrom 9500]
    test "foobytes=9500-" Nothing
    test "bytes=0-0,-1" $ Just [HH.ByteRangeFromTo 0 0, HH.ByteRangeSuffix 1]
  where
    blankSafe = (sourceList ["f", "oo\n", "bar\nbaz\n\r\n"]) $$ headerLines
    whiteSafe = (sourceList ["foo\r\nbar\r\nbaz\r\n\r\n hi there"]) $$ headerLines
    tooMany = (sourceList $ repeat "f\n") $$ headerLines
    tooLarge = (sourceList $ repeat "f") $$ headerLines

overLargeHeader :: Selector InvalidRequest
overLargeHeader e = e == OverLargeHeader
