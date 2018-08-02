{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.TimeoutSpec
    ( spec
    ) where

import Test.Hspec

import Control.Concurrent (threadDelay)
import Network.HTTP.Types (status200, status503, status504)
import Network.Wai
import Network.Wai.Middleware.Timeout
import Network.Wai.Test

spec :: Spec
spec = do
    describe "timeout" $ do
        it "times out slow requests with 503" $ do
            let app _req respond = do
                    threadDelay $ 2 * 1000000
                    respond $ responseLBS status200 [] ""

            resp <- runApp $ timeout 1 app

            simpleStatus resp `shouldBe` status503

        it "does not time out fast requests" $ do
            let app _req respond = respond $ responseLBS status200 [] ""

            resp <- runApp $ timeout 3 app

            simpleStatus resp `shouldBe` status200

    describe "timeoutStatus" $ do
        it "allows customizing the timeout response status" $ do
            let app _req respond = do
                    threadDelay $ 2 * 1000000
                    respond $ responseLBS status200 [] ""

            resp <- runApp $ timeoutStatus status504 1 app

            simpleStatus resp `shouldBe` status504

    describe "timeoutAs" $ do
        it "allows customizing the timeout response" $ do
            let app _req respond = do
                    threadDelay $ 2 * 1000000
                    respond $ responseLBS status200 [] ""
                timeoutResponse = responseLBS status503 [("X-Timeout", "1")] ""

            resp <- runApp $ timeoutAs timeoutResponse 1 app

            simpleStatus resp `shouldBe` status503
            simpleHeaders resp `shouldBe` [("X-Timeout", "1")]

runApp :: Application -> IO SResponse
runApp = runSession $ request defaultRequest
