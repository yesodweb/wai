{-# LANGUAGE OverloadedStrings #-}

module ResponseHeaderSpec (main, spec) where

import Data.ByteString
import qualified Network.HTTP.Types as H
import Network.Wai.Handler.Warp.ResponseHeader
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "composeHeader" $ do
    it "composes a HTTP header" $
        composeHeader H.http11 H.ok200 headers `shouldReturn` composedHeader

headers :: H.ResponseHeaders
headers = [
    ("Date", "Mon, 13 Aug 2012 04:22:55 GMT")
  , ("Content-Lenght", "151")
  , ("Server", "Mighttpd/2.5.8")
  , ("Last-Modified", "Fri, 22 Jun 2012 01:18:08 GMT")
  , ("Content-Type", "text/html")
  ]

composedHeader :: ByteString
composedHeader = "HTTP/1.1 200 OK\r\nDate: Mon, 13 Aug 2012 04:22:55 GMT\r\nContent-Lenght: 151\r\nServer: Mighttpd/2.5.8\r\nLast-Modified: Fri, 22 Jun 2012 01:18:08 GMT\r\nContent-Type: text/html\r\n\r\n"
