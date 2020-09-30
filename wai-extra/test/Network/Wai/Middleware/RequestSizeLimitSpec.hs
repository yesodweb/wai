{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.RequestSizeLimitSpec (main, spec) where

import Test.Hspec

import Network.Wai
import Network.Wai.Test
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import Network.Wai.Middleware.RequestSizeLimit
import Network.HTTP.Types.Status (status200, status413, Status)
import Control.Monad (replicateM)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "RequestSizeLimitMiddleware" $ do
  runStrictBodyTests "returns 413 for request bodies > 10 bytes, when streaming the whole body" "1234567890a" status413
  runStrictBodyTests "returns 200 for request bodies <= 10 bytes, when streaming the whole body" "1234567890" status200

  describe "streaming chunked bodies" $ do
    let streamingReq = defaultRequest
                    { isSecure = False
                    , requestBodyLength = ChunkedBody
                    , requestBody = return "a"
                    }
    it "413s if the combined chunk size is > the size limit" $ do
      resp <- runStreamingChunkApp 11 (requestSizeLimitMiddleware 10) streamingReq
      simpleStatus resp `shouldBe` status413
    it "200s if the combined chunk size is <= the size limit" $ do
      resp <- runStreamingChunkApp 10 (requestSizeLimitMiddleware 10) streamingReq
      simpleStatus resp `shouldBe` status200

data LengthType = UseKnownLength | UseChunked
  deriving (Show, Eq)  

runStrictBodyTests :: String -> ByteString -> Status -> Spec
runStrictBodyTests name requestBody expectedStatus = describe name $ do
  it "chunked" $ do
    let req = mkRequestWithBytestring requestBody UseChunked
    resp <- runStrictBodyApp (requestSizeLimitMiddleware 10) req

    simpleStatus resp `shouldBe` expectedStatus
  it "non-chunked" $ do
    let req = mkRequestWithBytestring requestBody UseKnownLength
    resp <- runStrictBodyApp (requestSizeLimitMiddleware 10) req

    simpleStatus resp `shouldBe` expectedStatus
  where
    mkRequestWithBytestring :: ByteString -> LengthType -> SRequest
    mkRequestWithBytestring body lengthType = SRequest defaultRequest
      { requestHeaders =
          if lengthType == UseKnownLength
              then [("content-length", S8.pack $ show $ S.length body)]
              else []
      , requestMethod = "POST"
      , requestBodyLength =
          if lengthType == UseKnownLength
              then KnownLength $ fromIntegral $ S.length body
              else ChunkedBody
      } $ L.fromChunks $ map S.singleton $ S.unpack body

runStrictBodyApp :: Middleware -> SRequest -> IO SResponse
runStrictBodyApp mw req = runSession
    (srequest req) $ mw app
  where
    app req respond = do
      _body <- strictRequestBody req
      respond $ responseLBS status200 [] ""

runStreamingChunkApp :: Int -> Middleware -> Request -> IO SResponse
runStreamingChunkApp times mw req = runSession
    (request req) $ mw app
  where
    app req respond = do
      _chunks <- replicateM times (getRequestBodyChunk req)
      respond $ responseLBS status200 [] ""
