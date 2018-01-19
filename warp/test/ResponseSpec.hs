{-# LANGUAGE OverloadedStrings #-}

module ResponseSpec (main, spec) where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Maybe (mapMaybe)
import Network.HTTP.Types
import Network.Wai hiding (responseHeaders)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Response
import RunSpec (withApp)
import System.IO (hClose, hFlush)
import Test.Hspec

-- import HTTP
import RunSpec (connectTo)

main :: IO ()
main = hspec spec

testRange :: S.ByteString -- ^ range value
          -> String -- ^ expected output
          -> Maybe String -- ^ expected content-range value
          -> Spec
testRange range out crange = it title $ withApp defaultSettings app $ \port -> do
    handle <- connectTo "127.0.0.1" port
    S.hPutStr handle "GET / HTTP/1.0\r\n"
    S.hPutStr handle "Range: bytes="
    S.hPutStr handle range
    S.hPutStr handle "\r\n\r\n"
    hFlush handle
    threadDelay 10000
    bss <- fmap (lines . filter (/= '\r') . S8.unpack) $ S.hGetSome handle 1024
    hClose handle
    last bss `shouldBe` out
    let hs = mapMaybe toHeader bss
    lookup "Content-Range" hs `shouldBe` fmap ("bytes " ++) crange
    lookup "Content-Length" hs `shouldBe` Just (show $ length $ last bss)
  where
    app _ = ($ responseFile status200 [] "attic/hex" Nothing)
    title = show (range, out, crange)
    toHeader s =
        case break (== ':') s of
            (x, ':':y) -> Just (x, dropWhile (== ' ') y)
            _ -> Nothing

testPartial :: Integer -- ^ file size
            -> Integer -- ^ offset
            -> Integer -- ^ byte count
            -> String -- ^ expected output
            -> Spec
testPartial size offset count out = it title $ withApp defaultSettings app $ \port -> do
    handle <- connectTo "127.0.0.1" port
    S.hPutStr handle "GET / HTTP/1.0\r\n\r\n"
    hFlush handle
    threadDelay 10000
    bss <- fmap (lines . filter (/= '\r') . S8.unpack) $ S.hGetSome handle 1024
    hClose handle
    out `shouldBe` last bss
    let hs = mapMaybe toHeader bss
    lookup "Content-Length" hs `shouldBe` Just (show $ length $ last bss)
    lookup "Content-Range" hs `shouldBe` Just range
  where
    app _ = ($ responseFile status200 [] "attic/hex" $ Just $ FilePart offset count size)
    title = show (offset, count, out)
    toHeader s =
        case break (== ':') s of
            (x, ':':y) -> Just (x, dropWhile (== ' ') y)
            _ -> Nothing
    range = "bytes " ++ show offset ++ "-" ++ show (offset + count - 1) ++ "/" ++ show size

spec :: Spec
spec = do
{- http-client does not support this.
    describe "preventing response splitting attack" $ do
        it "sanitizes header values" $ do
            let app _ respond = respond $ responseLBS status200 [("foo", "foo\r\nbar")] "Hello"
            withApp defaultSettings app $ \port -> do
                res <- sendGET $ "http://127.0.0.1:" ++ show port
                getHeaderValue "foo" (responseHeaders res) `shouldBe`
                  Just "foo   bar" -- HTTP inserts two spaces for \r\n.
-}

    describe "sanitizeHeaderValue" $ do
        it "doesn't alter valid multiline header values" $ do
            sanitizeHeaderValue "foo\r\n bar" `shouldBe` "foo\r\n bar"

        it "adds missing spaces after \r\n" $ do
            sanitizeHeaderValue "foo\r\nbar" `shouldBe` "foo\r\n bar"

        it "discards empty lines" $ do
            sanitizeHeaderValue "foo\r\n\r\nbar" `shouldBe` "foo\r\n bar"

        context "when sanitizing single occurences of \n" $ do
            it "replaces \n with \r\n" $ do
                sanitizeHeaderValue "foo\n bar" `shouldBe` "foo\r\n bar"

            it "adds missing spaces after \n" $ do
                sanitizeHeaderValue "foo\nbar" `shouldBe` "foo\r\n bar"

        it "discards single occurrences of \r" $ do
            sanitizeHeaderValue "foo\rbar" `shouldBe` "foobar"

    describe "range requests" $ do
        testRange "2-3" "23" $ Just "2-3/16"
        testRange "5-" "56789abcdef" $ Just "5-15/16"
        testRange "5-8" "5678" $ Just "5-8/16"
        testRange "-3" "def" $ Just "13-15/16"
        testRange "16-" "" $ Just "*/16"
        testRange "-17" "0123456789abcdef" Nothing

    describe "partial files" $ do
        testPartial 16 2 2 "23"
        testPartial 16 0 2 "01"
        testPartial 16 3 8 "3456789a"
