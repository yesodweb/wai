{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.RequestSpec
    ( main
    , spec
    ) where

import Test.Hspec

import Data.ByteString (ByteString)
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request(..), defaultRequest)

import Network.Wai.Request

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "appearsSecure" $ do
    let insecureRequest = defaultRequest
            { isSecure = False
            , requestHeaders =
                [ ("HTTPS", "off")
                , ("HTTP_X_FORWARDED_SSL", "off")
                , ("HTTP_X_FORWARDED_SCHEME", "http")
                , ("HTTP_X_FORWARDED_PROTO", "http,xyz")
                ]
            }

    it "returns False for an insecure request" $
        insecureRequest `shouldSatisfy` not . appearsSecure

    it "checks if the Request is actually secure" $ do
        let req = insecureRequest { isSecure = True }

        req `shouldSatisfy` appearsSecure

    it "checks for HTTP: on" $ do
        let req = addHeader "HTTPS" "on" insecureRequest

        req `shouldSatisfy` appearsSecure

    it "checks for HTTP_X_FORWARDED_SSL: on" $ do
        let req = addHeader "HTTP_X_FORWARDED_SSL" "on" insecureRequest

        req `shouldSatisfy` appearsSecure

    it "checks for HTTP_X_FORWARDED_SCHEME: https" $ do
        let req = addHeader "HTTP_X_FORWARDED_SCHEME" "https" insecureRequest

        req `shouldSatisfy` appearsSecure

    it "checks for HTTP_X_FORWARDED_PROTO: https,..." $ do
        let req = addHeader "HTTP_X_FORWARDED_PROTO" "https,xyz" insecureRequest

        req `shouldSatisfy` appearsSecure

addHeader :: HeaderName -> ByteString -> Request -> Request
addHeader name value req = req
    { requestHeaders = (name, value) : otherHeaders }

  where
    otherHeaders = filter ((/= name) . fst) $ requestHeaders req
