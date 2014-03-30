{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}

module RequestSpec (main, spec) where

import Data.Conduit
import qualified Data.Conduit.Binary as CB
import Data.Conduit.List (sourceList)
import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.RequestHeader (parseByteRanges)
import Network.Wai.Handler.Warp.Types
import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Network.HTTP.Types.Header as HH

deriving instance Show HH.ByteRange
deriving instance Eq HH.ByteRange

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "headerLines" $ do
    it "takes until blank" $
        blankSafe >>= (`shouldBe` (Nothing, ["foo", "bar", "baz"]))
    it "ignored leading whitespace in bodies" $
        whiteSafe >>= (`shouldBe` (Just " hi there", ["foo", "bar", "baz"]))
    it "throws OverLargeHeader when too many" $
        tooMany `shouldThrow` overLargeHeader
    it "throws OverLargeHeader when too large" $
        tooLarge `shouldThrow` overLargeHeader
    it "known bad chunking behavior #239" $ do
        let chunks =
                [ "GET / HTTP/1.1\r\nConnection: Close\r"
                , "\n\r\n"
                ]
        (mleftover, actual) <- mapM_ yield chunks $$ headerLines
        mleftover `shouldBe` Nothing
        actual `shouldBe` ["GET / HTTP/1.1", "Connection: Close"]
    prop "random chunking" $ \breaks extraS -> do
        let bsFull = "GET / HTTP/1.1\r\nConnection: Close\r\n\r\n" `S8.append` extra
            extra = S8.pack extraS
            chunks = loop breaks bsFull
            loop [] bs = [bs, undefined]
            loop (x:xs) bs =
                bs1 : loop xs bs2
              where
                (bs1, bs2) = S8.splitAt ((x `mod` 10) + 1) bs
        (leftover, actual) <- mapM_ yield chunks $$ do
            (_, actual) <- headerLines
            x' <- CB.take (length extraS)
            let x = S8.concat $ L.toChunks x'
            return (x, actual)
        actual `shouldBe` ["GET / HTTP/1.1", "Connection: Close"]
        leftover `shouldBe` extra
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
