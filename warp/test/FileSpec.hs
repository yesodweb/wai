{-# LANGUAGE OverloadedStrings #-}

module FileSpec (main, spec) where

import Network.HTTP.Types
import Network.Wai.Handler.Warp.File
import Network.Wai.Handler.Warp.FileInfoCache
import Network.Wai.Handler.Warp.Header

import Test.Hspec

main :: IO ()
main = hspec spec

testFileRange :: String
              -> RequestHeaders -> FilePath
              -> RspFileInfo
              -> Spec
testFileRange desc reqhs file ans = it desc $ do
    finfo <- getInfo file
    let WithBody s hs off len = ans
        hs' = ("Last-Modified",fileInfoDate finfo) : hs
        ans' = WithBody s hs' off len
    conditionalRequest finfo [] (indexRequestHeader reqhs) `shouldBe` ans'

spec :: Spec
spec = do
    describe "conditionalRequest" $ do
        testFileRange
            "gets a file size from file system"
            [] "attic/hex"
            $ WithBody ok200 [("Content-Length","16"),("Accept-Ranges","bytes")] 0 16
        testFileRange
            "gets a file size from file system and handles Range and returns Partical Content"
            [("Range","bytes=2-14")] "attic/hex"
            $ WithBody status206 [("Content-Range","bytes 2-14/16"),("Content-Length","13"),("Accept-Ranges","bytes")] 2 13
        testFileRange
            "truncates end point of range to file size"
            [("Range","bytes=10-20")] "attic/hex"
            $ WithBody status206 [("Content-Range","bytes 10-15/16"),("Content-Length","6"),("Accept-Ranges","bytes")] 10 6
        testFileRange
            "gets a file size from file system and handles Range and returns OK if Range means the entire"
            [("Range:","bytes=0-15")] "attic/hex"
            $ WithBody status200 [("Content-Length","16"),("Accept-Ranges","bytes")] 0 16

