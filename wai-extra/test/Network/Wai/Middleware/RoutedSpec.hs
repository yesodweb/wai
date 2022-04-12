{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.RoutedSpec
    ( main
    , spec
    ) where

import Data.ByteString (ByteString)
import Data.String (IsString)
import Network.HTTP.Types (hContentType, status200)
import Network.Wai
import Network.Wai.Test
import Test.Hspec

import Network.Wai.Middleware.Routed
import Network.Wai.Middleware.ForceSSL (forceSSL)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "forceSSL" $ do
  it "routed middleware" $ do
    let destination = "https://example.com/d/"
    let routedSslJsonApp prefix = routedMiddleware (checkPrefix prefix) forceSSL jsonApp
        checkPrefix p (p1:_) = p == p1
        checkPrefix _ _ = False

    flip runSession (routedSslJsonApp "r") $ do
        res <- testDPath "http"
        assertNoHeader location res
        assertStatus 200 res
        assertBody "{\"foo\":\"bar\"}" res

    flip runSession (routedSslJsonApp "d") $ do
        res2 <- testDPath "http"
        assertHeader location destination res2
        assertStatus 301 res2

jsonApp :: Application
jsonApp _req cps = cps $ responseLBS status200
   [(hContentType, "application/json")]
      "{\"foo\":\"bar\"}"

testDPath :: ByteString -> Session SResponse
testDPath proto =
    request $ flip setRawPathInfo "/d/" defaultRequest
             { requestHeaders = [("X-Forwarded-Proto", proto)]
             , requestHeaderHost = Just "example.com"
             }

location :: IsString ci => ci
location = "Location"
