{-# LANGUAGE OverloadedStrings #-}
module ResponseSpec (main, spec) where

import Control.Concurrent (threadDelay)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.Maybe (mapMaybe)
import Network (connectTo, PortID (PortNumber))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Response
import RunSpec (withApp)
import System.IO (hClose, hFlush)
import Test.Hspec

main :: IO ()
main = hspec spec

testRange :: S.ByteString -- ^ range value
          -> String -- ^ expected output
          -> Maybe String -- ^ expected content-range value
          -> Spec
testRange range out crange = it title $ withApp defaultSettings app $ \port -> do
    handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
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
    handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
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

testFileRange :: String
              -> Status -> ResponseHeaders -> FilePath
              -> Maybe FilePart -> Maybe HeaderValue
              -> Either String (Status, ResponseHeaders, Integer, Integer)
              -> Spec
testFileRange desc s rsphdr file mPart mRange ans = it desc $ do
    eres <- fileRange s rsphdr file mPart mRange
    let res = case eres of
            Left  e   -> Left $ show e
            Right r   -> Right r
    res `shouldBe` ans

spec :: Spec
spec = do
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

    describe "fileRange" $ do
        testFileRange
            "gets a file size from file system"
            status200 [] "attic/hex" Nothing Nothing
            $ Right (status200,[("Content-Length","16")],0,16)
        testFileRange
            "gets an error if a file does not exist"
            status200 [] "attic/nonexist" Nothing Nothing
            $ Left "attic/nonexist: getFileStatus: does not exist (No such file or directory)"
        testFileRange
            "changes status if FileParts is specified"
            status200 [] "attic/hex" (Just (FilePart 2 10 16)) Nothing
            $ Right (status206,[("Content-Range", "bytes 2-11/16"),("Content-Length","10")],2,10)
        testFileRange
            "does not change status and does not add Content-Range if FileParts means the entire"
            status200 [] "attic/hex" (Just (FilePart 0 16 16)) Nothing
            $ Right (status200,[("Content-Length","16")],0,16)
        testFileRange
            "gets a file size from file system and handles Range and returns Partical Content"
            status200 [] "attic/hex" Nothing (Just "bytes=2-14")
            $ Right (status206,[("Content-Range","bytes 2-14/16"),("Content-Length","13")],2,13)
        testFileRange
            "truncates end point of range to file size"
            status200 [] "attic/hex" Nothing (Just "bytes=10-20")
            $ Right (status206,[("Content-Range","bytes 10-15/16"),("Content-Length","6")],10,6)
        testFileRange
            "gets a file size from file system and handles Range and returns OK if Range means the entire"
            status200 [] "attic/hex" Nothing (Just "bytes=0-15")
            $ Right (status200,[("Content-Length","16")],0,16)
        testFileRange
            "igores Range if FilePart is specified"
            status200 [] "attic/hex" (Just (FilePart 2 10 16)) (Just "bytes=8-9")
            $ Right (status206,[("Content-Range", "bytes 2-11/16"),("Content-Length","10")],2,10)
