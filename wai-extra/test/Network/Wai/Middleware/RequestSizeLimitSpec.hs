{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.RequestSizeLimitSpec (main, spec) where

import Control.Monad (replicateM)
import Data.Aeson (encode, object, (.=))
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Text (Text)
import Network.HTTP.Types (hContentLength, status200, status413)
import Network.Wai
import Test.Hspec

import Network.Wai.Middleware.RequestSizeLimit
import Network.Wai.Test

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "RequestSizeLimitMiddleware" $ do
    describe "Plain text response" $ do
        runStrictBodyTests
            "returns 413 for request bodies > 10 bytes, when streaming the whole body"
            tenByteLimitSettings
            "1234567890a"
            isStatus413
        runStrictBodyTests
            "returns 200 for request bodies <= 10 bytes, when streaming the whole body"
            tenByteLimitSettings
            "1234567890"
            isStatus200

    describe "JSON response" $ do
        runStrictBodyTests
            "returns 413 for request bodies > 10 bytes, when streaming the whole body"
            tenByteLimitJSONSettings
            "1234567890a"
            (isStatus413 >> isJSONContentType)
        runStrictBodyTests
            "returns 200 for request bodies <= 10 bytes, when streaming the whole body"
            tenByteLimitJSONSettings
            "1234567890"
            isStatus200

    describe "Per-request sizes" $ do
        it "allows going over the limit, when the path has been whitelisted" $ do
            let req =
                    SRequest
                        defaultRequest
                            { pathInfo = ["upload", "image"]
                            }
                        "1234567890a"
                settings =
                    setMaxLengthForRequest
                        ( \req' ->
                            if pathInfo req' == ["upload", "image"] then pure $ Just 20 else pure $ Just 10
                        )
                        defaultRequestSizeLimitSettings
            resp <- runStrictBodyApp settings req
            isStatus200 resp

    describe "streaming chunked bodies" $ do
        let streamingReq =
                setRequestBodyChunks
                    (return "a")
                    defaultRequest
                        { isSecure = False
                        , requestBodyLength = ChunkedBody
                        }
        it "413s if the combined chunk size is > the size limit" $ do
            resp <- runStreamingChunkApp 11 tenByteLimitSettings streamingReq
            simpleStatus resp `shouldBe` status413
        it "200s if the combined chunk size is <= the size limit" $ do
            resp <- runStreamingChunkApp 10 tenByteLimitSettings streamingReq
            simpleStatus resp `shouldBe` status200
  where
    tenByteLimitSettings =
        setMaxLengthForRequest
            (\_req -> pure $ Just 10)
            defaultRequestSizeLimitSettings
    tenByteLimitJSONSettings =
        setOnLengthExceeded
            ( \_maxLen _app _req sendResponse ->
                sendResponse $
                    responseLBS
                        status413
                        [("Content-Type", "application/json")]
                        (encode $ object ["error" .= ("request size too large" :: Text)])
            )
            tenByteLimitSettings

    isStatus413 = \sResp -> simpleStatus sResp `shouldBe` status413
    isStatus200 = \sResp -> simpleStatus sResp `shouldBe` status200
    isJSONContentType = \sResp -> simpleHeaders sResp `shouldBe` [("Content-Type", "application/json")]

data LengthType = UseKnownLength | UseChunked
    deriving (Show, Eq)

runStrictBodyTests
    :: String
    -> RequestSizeLimitSettings
    -> ByteString
    -> (SResponse -> Expectation)
    -> Spec
runStrictBodyTests name settings reqBody runExpectations = describe name $ do
    it "chunked" $ do
        let req = mkRequestWithBytestring reqBody UseChunked
        resp <- runStrictBodyApp settings req

        runExpectations resp
    it "non-chunked" $ do
        let req = mkRequestWithBytestring reqBody UseKnownLength
        resp <- runStrictBodyApp settings req

        runExpectations resp
  where
    mkRequestWithBytestring :: ByteString -> LengthType -> SRequest
    mkRequestWithBytestring body lengthType =
        SRequest adjustedRequest $
            L.fromChunks $
                map S.singleton $
                    S.unpack body
      where
        adjustedRequest =
            defaultRequest
                { requestHeaders =
                    [ (hContentLength, S8.pack $ show $ S.length body)
                    | lengthType == UseKnownLength
                    ]
                , requestMethod = "POST"
                , requestBodyLength =
                    if lengthType == UseKnownLength
                        then KnownLength $ fromIntegral $ S.length body
                        else ChunkedBody
                }

runStrictBodyApp :: RequestSizeLimitSettings -> SRequest -> IO SResponse
runStrictBodyApp settings req =
    runSession (srequest req) $
        requestSizeLimitMiddleware settings app
  where
    app req' respond = do
        _body <- strictRequestBody req'
        respond $ responseLBS status200 [] ""

runStreamingChunkApp
    :: Int -> RequestSizeLimitSettings -> Request -> IO SResponse
runStreamingChunkApp times settings req =
    runSession (request req) $
        requestSizeLimitMiddleware settings app
  where
    app req' respond = do
        _chunks <- replicateM times (getRequestBodyChunk req')
        respond $ responseLBS status200 [] ""
