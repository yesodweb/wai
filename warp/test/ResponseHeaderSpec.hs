{-# LANGUAGE OverloadedStrings #-}

module ResponseHeaderSpec (main, spec) where

import Data.ByteString
import qualified Network.HTTP.Types as H
import Network.Wai.Handler.Warp.ResponseHeader
import Network.Wai.Handler.Warp.Response
import Network.Wai.Handler.Warp.Header
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "composeHeader" $ do
        it "composes a HTTP header" $
            composeHeader H.http11 H.ok200 headers `shouldReturn` composedHeader
    describe "addServer" $ do
        it "adds Server if not exist" $ do
            let hdrs = []
                rspidxhdr = indexResponseHeader hdrs
            addServer "MyServer" rspidxhdr hdrs `shouldBe` [("Server","MyServer")]
        it "does not add Server if exists" $ do
            let hdrs = [("Server","MyServer")]
                rspidxhdr = indexResponseHeader hdrs
            addServer "MyServer2" rspidxhdr hdrs `shouldBe` hdrs
        it "does not add Server if empty" $ do
            let hdrs = []
                rspidxhdr = indexResponseHeader hdrs
            addServer "" rspidxhdr hdrs `shouldBe` hdrs
        it "deletes Server " $ do
            let hdrs = [("Server","MyServer")]
                rspidxhdr = indexResponseHeader hdrs
            addServer "" rspidxhdr hdrs `shouldBe` []

headers :: H.ResponseHeaders
headers = [
    ("Date", "Mon, 13 Aug 2012 04:22:55 GMT")
  , ("Content-Length", "151")
  , ("Server", "Mighttpd/2.5.8")
  , ("Last-Modified", "Fri, 22 Jun 2012 01:18:08 GMT")
  , ("Content-Type", "text/html")
  ]

composedHeader :: ByteString
composedHeader = "HTTP/1.1 200 OK\r\nDate: Mon, 13 Aug 2012 04:22:55 GMT\r\nContent-Length: 151\r\nServer: Mighttpd/2.5.8\r\nLast-Modified: Fri, 22 Jun 2012 01:18:08 GMT\r\nContent-Type: text/html\r\n\r\n"
