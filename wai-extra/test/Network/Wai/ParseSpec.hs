{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.ParseSpec (main, spec) where

import           Test.Hspec
import           Test.HUnit

import           System.IO
import           Data.Monoid
import qualified Data.IORef as I
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as TS
import qualified Data.Text.Encoding as TE
import           Control.Monad.Trans.Resource (withInternalState, runResourceT)

import           Network.Wai
import           Network.Wai.Test
import           Network.Wai.Parse
import           WaiExtraSpec (toRequest)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "parseContentType" $ do
        let go (x, y, z) = it (TS.unpack $ TE.decodeUtf8 x) $ parseContentType x `shouldBe` (y, z)
        mapM_ go
            [ ("text/plain", "text/plain", [])
            , ("text/plain; charset=UTF-8 ", "text/plain", [("charset", "UTF-8")])
            , ("text/plain; charset=UTF-8 ; boundary = foo", "text/plain", [("charset", "UTF-8"), ("boundary", "foo")])
            ]
    it "parseHttpAccept" caseParseHttpAccept
    describe "parseRequestBody" $ do
      caseParseRequestBody
    it "multipart with plus" caseMultipartPlus
    it "multipart with multiple attributes" caseMultipartAttrs
    it "urlencoded with plus" caseUrlEncPlus
    describe "dalvik multipart" $ do
        it "non-chunked" $ dalvikHelper True
        it "chunked" $ dalvikHelper False

caseParseHttpAccept :: Assertion
caseParseHttpAccept = do
    let input = "text/plain; q=0.5, text/html;charset=utf-8, text/*;q=0.8;ext=blah, text/x-dvi; q=0.8, text/x-c"
        expected = ["text/html;charset=utf-8", "text/x-c", "text/x-dvi", "text/*", "text/plain"]
    expected @=? parseHttpAccept input

parseRequestBody' :: BackEnd file
                  -> SRequest
                  -> IO ([(S.ByteString, S.ByteString)], [(S.ByteString, FileInfo file)])
parseRequestBody' sink (SRequest req bod) =
    case getRequestBodyType req of
        Nothing -> return ([], [])
        Just rbt -> do
            ref <- I.newIORef $ L.toChunks bod
            let rb = I.atomicModifyIORef ref $ \chunks ->
                        case chunks of
                            [] -> ([], S.empty)
                            x:y -> (y, x)
            sinkRequestBody sink rbt rb

caseParseRequestBody :: Spec
caseParseRequestBody = do
  it "parsing post x-www-form-urlencoded" $ do
    let content1 = "foo=bar&baz=bin"
    let ctype1 = "application/x-www-form-urlencoded"
    result1 <- parseRequestBody' lbsBackEnd $ toRequest ctype1 content1
    result1 `shouldBe` ([("foo", "bar"), ("baz", "bin")], [])

  let ctype2 = "multipart/form-data; boundary=AaB03x"
  let expectedsmap2 =
          [ ("title", "A File")
          , ("summary", "This is my file\nfile test")
          ]
  let textPlain = "text/plain; charset=iso-8859-1"
  let expectedfile2 =
          [("document", FileInfo "b.txt" textPlain "This is a file.\nIt has two lines.")]
  let expected2 = (expectedsmap2, expectedfile2)

  it "parsing post multipart/form-data" $ do
    result2 <- parseRequestBody' lbsBackEnd $ toRequest ctype2 content2
    result2 `shouldBe` expected2

  it "parsing post multipart/form-data 2" $ do
    result2' <- parseRequestBody' lbsBackEnd $ toRequest' ctype2 content2
    result2' `shouldBe` expected2


  let ctype3 = "multipart/form-data; boundary=----WebKitFormBoundaryB1pWXPZ6lNr8RiLh"
  let expectedsmap3 = []
  let expectedfile3 = [("yaml", FileInfo "README" "application/octet-stream" "Photo blog using Hack.\n")]
  let expected3 = (expectedsmap3, expectedfile3)

  it "parsing actual post multipart/form-data" $ do
    result3 <- parseRequestBody' lbsBackEnd $ toRequest ctype3 content3
    result3 `shouldBe` expected3

  it "parsing actual post multipart/form-data 2" $ do
    result3' <- parseRequestBody' lbsBackEnd $ toRequest' ctype3 content3
    result3' `shouldBe` expected3
  where
    content2 =
         "--AaB03x\n"
      <> "Content-Disposition: form-data; name=\"document\"; filename=\"b.txt\"\n"
      <> "Content-Type: text/plain; charset=iso-8859-1\n\n"
      <> "This is a file.\n"
      <> "It has two lines.\n"
      <> "--AaB03x\n"
      <> "Content-Disposition: form-data; name=\"title\"\n"
      <> "Content-Type: text/plain; charset=iso-8859-1\n\n"
      <> "A File\n"
      <> "--AaB03x\n"
      <> "Content-Disposition: form-data; name=\"summary\"\n"
      <> "Content-Type: text/plain; charset=iso-8859-1\n\n"
      <> "This is my file\n"
      <> "file test\n"
      <> "--AaB03x--"
    content3 =
         "------WebKitFormBoundaryB1pWXPZ6lNr8RiLh\r\n"
      <> "Content-Disposition: form-data; name=\"yaml\"; filename=\"README\"\r\n"
      <> "Content-Type: application/octet-stream\r\n\r\n"
      <> "Photo blog using Hack.\n\r\n"
      <> "------WebKitFormBoundaryB1pWXPZ6lNr8RiLh--\r\n"

caseMultipartPlus :: Assertion
caseMultipartPlus = do
    result <- parseRequestBody' lbsBackEnd $ toRequest ctype content
    result @?= ([("email", "has+plus")], [])
  where
    content =
        "--AaB03x\n" <>
        "Content-Disposition: form-data; name=\"email\"\n" <>
        "Content-Type: text/plain; charset=iso-8859-1\n\n" <>
        "has+plus\n" <>
        "--AaB03x--"
    ctype = "multipart/form-data; boundary=AaB03x"

caseMultipartAttrs :: Assertion
caseMultipartAttrs = do
    result <- parseRequestBody' lbsBackEnd $ toRequest ctype content
    result @?= ([("email", "has+plus")], [])
  where
    content =
        "--AaB03x\n" <>
        "Content-Disposition: form-data; name=\"email\"\n" <>
        "Content-Type: text/plain; charset=iso-8859-1\n\n" <>
        "has+plus\n" <>
        "--AaB03x--"
    ctype = "multipart/form-data; charset=UTF-8; boundary=AaB03x"

caseUrlEncPlus :: Assertion
caseUrlEncPlus = do
    result <- runResourceT $ withInternalState $ \state ->
              parseRequestBody' (tempFileBackEnd state) $ toRequest ctype content
    result @?= ([("email", "has+plus")], [])
  where
    content = "email=has%2Bplus"
    ctype = "application/x-www-form-urlencoded"

dalvikHelper :: Bool -> Assertion
dalvikHelper includeLength = do
    let headers' =
            [ ("content-type", "multipart/form-data;boundary=*****")
            , ("GATEWAY_INTERFACE", "CGI/1.1")
            , ("PATH_INFO", "/")
            , ("QUERY_STRING", "")
            , ("REMOTE_ADDR", "192.168.1.115")
            , ("REMOTE_HOST", "ganjizza")
            , ("REQUEST_URI", "http://192.168.1.115:3000/")
            , ("REQUEST_METHOD", "POST")
            , ("HTTP_CONNECTION", "Keep-Alive")
            , ("HTTP_COOKIE", "_SESSION=fgUGM5J/k6mGAAW+MMXIJZCJHobw/oEbb6T17KQN0p9yNqiXn/m/ACrsnRjiCEgqtG4fogMUDI+jikoFGcwmPjvuD5d+MDz32iXvDdDJsFdsFMfivuey2H+n6IF6yFGD")
            , ("HTTP_USER_AGENT", "Dalvik/1.1.0 (Linux; U; Android 2.1-update1; sdk Build/ECLAIR)")
            , ("HTTP_HOST", "192.168.1.115:3000")
            , ("HTTP_ACCEPT", "*, */*")
            , ("HTTP_VERSION", "HTTP/1.1")
            , ("REQUEST_PATH", "/")
            ]
        headers
            | includeLength = ("content-length", "12098") : headers'
            | otherwise = headers'
    let request' = defaultRequest
            { requestHeaders = headers
            }
    (params, files) <-
        case getRequestBodyType request' of
            Nothing -> return ([], [])
            Just rbt -> withFile "test/requests/dalvik-request" ReadMode $ \h ->
                sinkRequestBody lbsBackEnd rbt $ S.hGetSome h 2048
    lookup "scannedTime" params @?= Just "1.298590056748E9"
    lookup "geoLong" params @?= Just "0"
    lookup "geoLat" params @?= Just "0"
    length files @?= 1

toRequest' :: S8.ByteString -> S8.ByteString -> SRequest
toRequest' ctype content = SRequest defaultRequest
    { requestHeaders = [("Content-Type", ctype)]
    } (L.fromChunks $ map S.singleton $ S.unpack content)
