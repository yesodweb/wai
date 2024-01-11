{-# LANGUAGE OverloadedStrings #-}

module FileSpec (main, spec) where

import Data.ByteString
import Data.String (fromString)
import Network.HTTP.Types
import Network.Wai.Handler.Warp.File
import Network.Wai.Handler.Warp.FileInfoCache
import Network.Wai.Handler.Warp.Header
import System.IO.Unsafe (unsafePerformIO)

import Test.Hspec

main :: IO ()
main = hspec spec

changeHeaders
    :: (ResponseHeaders -> ResponseHeaders) -> RspFileInfo -> RspFileInfo
changeHeaders f rfi =
    case rfi of
        WithBody s hs off len -> WithBody s (f hs) off len
        other -> other

getHeaders :: RspFileInfo -> ResponseHeaders
getHeaders rfi =
    case rfi of
        WithBody _ hs _ _ -> hs
        _ -> []

testFileRange
    :: String
    -> RequestHeaders
    -> RspFileInfo
    -> Spec
testFileRange desc reqhs ans = it desc $ do
    finfo <- getInfo "attic/hex"
    let f = (:) ("Last-Modified", fileInfoDate finfo)
        hs = getHeaders ans
        ans' = changeHeaders f ans
    conditionalRequest
        finfo
        []
        methodGet
        (indexResponseHeader hs)
        (indexRequestHeader reqhs)
        `shouldBe` ans'

farPast, farFuture :: ByteString
farPast = "Thu, 01 Jan 1970 00:00:00 GMT"
farFuture = "Sun, 05 Oct 3000 00:00:00 GMT"

regularBody :: RspFileInfo
regularBody = WithBody ok200 [("Content-Length", "16"), ("Accept-Ranges", "bytes")] 0 16

make206Body :: Integer -> Integer -> RspFileInfo
make206Body start len =
    WithBody status206 [crHeader, lenHeader, ("Accept-Ranges", "bytes")] start len
  where
    lenHeader = ("Content-Length", fromString $ show len)
    crHeader =
        ( "Content-Range"
        , fromString $ "bytes " <> show start <> "-" <> show (start + len - 1) <> "/16"
        )

spec :: Spec
spec = do
    describe "conditionalRequest" $ do
        testFileRange
            "gets a file size from file system"
            []
            regularBody
        testFileRange
            "gets a file size from file system and handles Range and returns Partical Content"
            [("Range", "bytes=2-14")]
            $ make206Body 2 13
        testFileRange
            "truncates end point of range to file size"
            [("Range", "bytes=10-20")]
            $ make206Body 10 6
        testFileRange
            "gets a file size from file system and handles Range and returns OK if Range means the entire"
            [("Range:", "bytes=0-15")]
            regularBody
        testFileRange
            "returns a 412 if the file has been changed in the meantime"
            [("If-Unmodified-Since", farPast)]
            $ WithoutBody status412
        testFileRange
            "gets a file if the file has not been changed in the meantime"
            [("If-Unmodified-Since", farFuture)]
            regularBody
        testFileRange
            "ignores the If-Unmodified-Since header if an If-Match header is also present"
            [("If-Match", "SomeETag"), ("If-Unmodified-Since", farPast)]
            regularBody
        testFileRange
            "still gives only a range, even after conditionals"
            [ ("If-Match", "SomeETag")
            , ("If-Unmodified-Since", farPast)
            , ("Range", "bytes=10-20")
            ]
            $ make206Body 10 6
        testFileRange
            "gets a file if the file has been changed in the meantime"
            [("If-Modified-Since", farPast)]
            regularBody
        testFileRange
            "returns a 304 if the file has not been changed in the meantime"
            [("If-Modified-Since", farFuture)]
            $ WithoutBody status304
        testFileRange
            "ignores the If-Modified-Since header if an If-None-Match header is also present"
            [("If-None-Match", "SomeETag"), ("If-Modified-Since", farFuture)]
            regularBody
        testFileRange
            "still gives only a range, even after conditionals"
            [ ("If-None-Match", "SomeETag")
            , ("If-Modified-Since", farFuture)
            , ("Range", "bytes=10-13")
            ]
            $ make206Body 10 4
        testFileRange
            "gives the a range, if the condition is met"
            [ ("If-Range", fileInfoDate (unsafePerformIO $ getInfo "attic/hex"))
            , ("Range", "bytes=2-7")
            ]
            $ make206Body 2 6
        testFileRange
            "gives the entire body and ignores the Range header if the condition isn't met"
            [("If-Range", farPast), ("Range", "bytes=2-7")]
            regularBody
