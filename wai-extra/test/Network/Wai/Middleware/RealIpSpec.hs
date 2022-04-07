{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.RealIpSpec
    ( spec
    ) where

import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.IP as IP
import Network.HTTP.Types (RequestHeaders, status200)
import Network.Wai
import Test.Hspec

import Network.Wai.Middleware.RealIp
import Network.Wai.Test

spec :: Spec
spec = do
    describe "realIp" $ do
        it "does nothing when header is missing" $ do
            resp <- runApp "127.0.0.1" [] realIp
            simpleBody resp `shouldBe` "127.0.0.1"

        it "uses X-Forwarded-For when present" $ do
            let headers = [("X-Forwarded-For", "1.1.1.1")]
            resp <- runApp "127.0.0.1" headers realIp
            simpleBody resp `shouldBe` "1.1.1.1"

        it "ignores X-Forwarded-For from non-trusted ip" $ do
            let headers = [("X-Forwarded-For", "1.1.1.1")]
            resp <- runApp "1.2.3.4" headers realIp
            simpleBody resp `shouldBe` "1.2.3.4"

        it "ignores trusted ip addresses in X-Forwarded-For" $ do
            let headers = [("X-Forwarded-For", "1.1.1.1, 10.0.0.1")]
            resp <- runApp "127.0.0.1" headers realIp
            simpleBody resp `shouldBe` "1.1.1.1"

        it "ignores invalid ip addresses" $ do
            let headers1 = [("X-Forwarded-For", "1.1.1")]
            resp1 <- runApp "127.0.0.1" headers1 realIp
            let headers2 = [("X-Forwarded-For", "1.1.1.1,foo")]
            resp2 <- runApp "127.0.0.1" headers2 realIp

            simpleBody resp1 `shouldBe` "127.0.0.1"
            simpleBody resp2 `shouldBe` "1.1.1.1"

        it "takes the last non-trusted address" $ do
            let headers = [("X-Forwarded-For", "1.1.1.1, 2.2.2.2, 10.0.0.1")]
            resp <- runApp "127.0.0.1" headers realIp
            simpleBody resp `shouldBe` "2.2.2.2"

        it "takes the first address when all are trusted" $ do
            let headers = [("X-Forwarded-For", "192.168.0.1, 10.0.0.2, 10.0.0.1")]
            resp <- runApp "127.0.0.1" headers realIp
            simpleBody resp `shouldBe` "192.168.0.1"

        it "handles repeated headers" $ do
            let headers = [("X-Forwarded-For", "1.1.1.1"), ("X-Forwarded-For", "2.2.2.2")]
            resp <- runApp "127.0.0.1" headers realIp
            simpleBody resp `shouldBe` "2.2.2.2"

        it "handles ipv6 addresses" $ do
            let headers = [("X-Forwarded-For", "2001:db8::ff00:42:8329,10.0.0.1")]
            resp <- runApp "::1" headers realIp
            simpleBody resp `shouldBe` "2001:db8::ff00:42:8329"

    describe "realIpHeader" $ do
        it "uses specified header" $ do
            let headers = [("X-Forwarded-For", "1.1.1.1"), ("X-Real-Ip", "2.2.2.2")]
            resp <- runApp "127.0.0.1" headers $ realIpHeader "X-Real-Ip"
            simpleBody resp `shouldBe` "2.2.2.2"

    describe "realIpTrusted" $ do
        it "uses provided trusted predicate" $ do
            let headers = [("X-Forwarded-For", "10.0.0.1, 1.1.1.1")]
                isTrusted1 ip = any (ipInRange ip) ["127.0.0.1/32", "1.1.1.1/32"]
                isTrusted2 = flip ipInRange "1.1.1.1/32"

            resp1 <- runApp "127.0.0.1" headers $ realIpTrusted "X-Forwarded-For" isTrusted1
            resp2 <- runApp "10.0.0.2" headers $ realIpTrusted "X-Forwarded-For" isTrusted2

            simpleBody resp1 `shouldBe` "10.0.0.1"
            simpleBody resp2 `shouldBe` "10.0.0.2"


runApp :: IP.IP -> RequestHeaders -> Middleware -> IO SResponse
runApp ip hs mw = runSession
    (request defaultRequest { remoteHost = IP.toSockAddr (ip, 80), requestHeaders = hs }) $ mw app
  where
    app req respond = respond $ responseLBS status200 [] $ renderIp req
    renderIp = B8.pack . maybe "" (show . fst) . IP.fromSockAddr . remoteHost
