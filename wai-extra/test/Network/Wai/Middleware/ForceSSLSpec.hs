{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.ForceSSLSpec
    ( main
    , spec
    ) where

import Test.Hspec

import Network.Wai.Middleware.ForceSSL

import Control.Monad
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Network.HTTP.Types (methodPost, status200, status301, status307)
import Network.Wai
import Network.Wai.Test

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "forceSSL" (forM_ hosts $ \host -> hostSpec host)
  where
    hosts = ["example.com", "example.com:80", "example.com:8080"]

hostSpec :: ByteString -> Spec
hostSpec host = describe ("forceSSL on host " <> show host <> "") $ do

    it "redirects non-https requests to https" $ do
        resp <- runApp host forceSSL defaultRequest

        simpleStatus resp `shouldBe` status301
        simpleHeaders resp `shouldBe` [("Location", "https://" <> host)]

    it "redirects with 307 in the case of a non-GET request" $ do
        resp <- runApp host forceSSL defaultRequest
            { requestMethod = methodPost }

        simpleStatus resp `shouldBe` status307
        simpleHeaders resp `shouldBe` [("Location", "https://" <> host)]

    it "does not redirect already-secure requests" $ do
        resp <- runApp host forceSSL defaultRequest { isSecure = True }

        simpleStatus resp `shouldBe` status200

    it "preserves the original host, path, and query string" $ do
        resp <- runApp host forceSSL defaultRequest
            { rawPathInfo = "/foo/bar"
            , rawQueryString = "?baz=bat"
            }

        simpleHeaders resp `shouldBe`
            [("Location", "https://" <> host <> "/foo/bar?baz=bat")]

runApp :: ByteString -> Middleware -> Request -> IO SResponse
runApp host mw req = runSession
    (request req { requestHeaderHost = Just host }) $ mw app
  where
    app _ respond = respond $ responseLBS status200 [] ""
