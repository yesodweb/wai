{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.RequestSizeLimitSpec (main, spec) where

import Test.Hspec

import Network.Wai
import Network.Wai.Test
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as S8
import Data.Text (Text)
import qualified Data.Text as T
import Network.Wai.Middleware.RequestSizeLimit
import Network.HTTP.Types.Status (status200, requestEntityTooLarge413)

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "RequestSizeLimitMiddleware" $ do

    it "rejects too large of requests" $ do
        let req = mkRequest "12lk;dfjdskljfaskl;jfsdkl;fjasklfjddk" True
        resp <- runApp (requestSizeLimit 10) req

        simpleStatus resp `shouldBe` requestEntityTooLarge413
    it "rejects too large of requests chunked" $ do
        let req = mkRequest "12lk;dfjdskljfaskl;jfsdkl;fjasklfjddk" False
        resp <- runApp (requestSizeLimit 10) req

        simpleStatus resp `shouldBe` requestEntityTooLarge413

    where
  
runTest :: String -> ByteString    

mkRequest body includeLength = SRequest defaultRequest
  { requestHeaders =
      if includeLength
          then [("content-length", S8.pack $ show $ S.length body)]
          else []
  , requestMethod = "POST"
  , requestBodyLength =
      if includeLength
          then KnownLength $ fromIntegral $ S.length body
          else ChunkedBody
  } $ L.fromChunks $ map S.singleton $ S.unpack body

runApp :: Middleware -> SRequest -> IO SResponse
runApp mw req = runSession
    (srequest req) $ mw app
  where
    app req respond = do
      _body <- strictRequestBody req
      putStrLn "Got body"
      respond $ responseLBS status200 [] ""



-- caseHelper :: String -- ^ name
--            -> Text -- ^ pathinfo
--            -> ByteString -- ^ request body
--            -> Int -- ^ expected status code, chunked
--            -> Int -- ^ expected status code, non-chunked
--            -> Spec
-- caseHelper name path body statusChunked statusNonChunked = describe name $ do
--     it "chunked" $ runner $ do
--         res <- mkRequest False
--         assertStatus statusChunked res
--     it "non-chunked" $ runner $ do
--         res <- mkRequest True
--         assertStatus statusNonChunked res
--   where
    -- mkRequest includeLength = srequest $ SRequest defaultRequest
    --     { pathInfo = [path]
    --     , requestHeaders =
    --         ("content-type", "application/x-www-form-urlencoded") :

    --         if includeLength
    --             then [("content-length", S8.pack $ show $ S.length body)]
    --             else []
    --     , requestMethod = "POST"
    --     , requestBodyLength =
    --         if includeLength
    --             then KnownLength $ fromIntegral $ S.length body
    --             else ChunkedBody
    --     } $ L.fromChunks $ map S.singleton $ S.unpack body

-- specs :: Spec
-- specs = describe "Test.RequestBodySize" $ do
--     caseHelper "lookupPostParam- large" "post" "foobarbaz=bin" 413 413
--     caseHelper "lookupPostParam- small" "post" "foo=bin" 200 200
--     caseHelper "total consume- large" "consume" "this is longer than 10" 413 413
--     caseHelper "total consume- small" "consume" "smaller" 200 200
--     caseHelper "partial consume- large" "partial-consume" "this is longer than 10" 200 413
--     caseHelper "partial consume- small" "partial-consume" "smaller" 200 200
--     caseHelper "unused- large" "unused" "this is longer than 10" 200 413
--     caseHelper "unused- small" "unused" "smaller" 200 200
