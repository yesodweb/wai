{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ApprootSpec (
    main,
    spec,
) where

import Data.ByteString (ByteString)
import Network.HTTP.Types (RequestHeaders, status200)
import Network.Wai
import Test.Hspec

import Network.Wai.Middleware.Approot (fromRequest, getApproot)
import Network.Wai.Test (SResponse (simpleHeaders), request, runSession)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    let test name host secure headers expected = it name $ do
            resp <- runApp host secure headers
            simpleHeaders resp `shouldBe` [("Approot", expected)]
    test "respects host header" "foobar" False [] "http://foobar"
    test "respects isSecure" "foobar" True [] "https://foobar"
    test
        "respects SSL headers"
        "foobar"
        False
        [("HTTP_X_FORWARDED_SSL", "on")]
        "https://foobar"

runApp :: ByteString -> Bool -> RequestHeaders -> IO SResponse
runApp host secure headers =
    runSession
        ( request
            defaultRequest
                { requestHeaderHost = Just host
                , isSecure = secure
                , requestHeaders = headers
                }
        )
        $ fromRequest app
  where
    app req respond = respond $ responseLBS status200 [("Approot", getApproot req)] ""
