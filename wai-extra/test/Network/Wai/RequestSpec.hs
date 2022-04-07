{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.RequestSpec
    ( main
    , spec
    ) where


import Control.Exception (try)
import Control.Monad (forever)
import Data.ByteString (ByteString)
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request (..), RequestBodyLength (..), defaultRequest)
import Network.Wai.Request
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "requestSizeCheck" $ do
        it "too large content length should throw RequestSizeException" $ do
            let limit = 1024
                largeRequest = defaultRequest
                    { isSecure = False
                    , requestBodyLength = KnownLength (limit + 1)
                    , requestBody = return "repeat this chunk"
                    }
            checkedRequest <- requestSizeCheck limit largeRequest
            body <- try (requestBody checkedRequest)
            case body of
                Left (RequestSizeException l) -> l `shouldBe` limit
                Right _   -> expectationFailure "request size check failed"

        it "too many chunks should throw RequestSizeException" $ do
            let limit = 1024
                largeRequest = defaultRequest
                    { isSecure = False
                    , requestBodyLength = ChunkedBody
                    , requestBody = return "repeat this chunk"
                    }
            checkedRequest <- requestSizeCheck limit largeRequest
            body <- try (forever $ requestBody checkedRequest)
            case body of
                Left (RequestSizeException l) -> l `shouldBe` limit
                Right _   -> expectationFailure "request size check failed"

    describe "appearsSecure" $ do
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
