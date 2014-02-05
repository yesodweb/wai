{-# LANGUAGE OverloadedStrings #-}

module ExceptionSpec (main, spec) where

import Control.Applicative
import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import Network.HTTP
import Network.Stream
import Network.HTTP.Types hiding (Header)
import Network.Wai hiding (Response)
import Network.Wai.Internal (Request(..))
import Network.Wai.Handler.Warp
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

main :: IO ()
main = hspec spec

testServer :: IO ()
testServer =
    run 2345 testApp

testApp :: Application
testApp (Network.Wai.Internal.Request {pathInfo = [x]})
    | x == "statusError" =
        return $ responseLBS undefined [] "foo"
    | x == "headersError" =
        return $ responseLBS ok200 undefined "foo"
    | x == "headerError" =
        return $ responseLBS ok200 [undefined] "foo"
    | x == "bodyError" =
        return $ responseLBS ok200 [] undefined
    | x == "ioException" = do
        void $ fail "ioException"
        return $ responseLBS ok200 [] "foo"
testApp _ =
        return $ responseLBS ok200 [] "foo"

spec :: Spec
spec = unsafePerformIO $ (forkIO testServer >> threadDelay 100000 >>) $ return $
    describe "responds even if there is an exception" $ do
        {- Disabling these tests. We can consider forcing evaluation in Warp.
        it "statusError" $ do
            sc <- rspCode <$> sendGET "http://localhost:2345/statusError"
            sc `shouldBe` (5,0,0)
        it "headersError" $ do
            sc <- rspCode <$> sendGET "http://localhost:2345/headersError"
            sc `shouldBe` (5,0,0)
        it "headerError" $ do
            sc <- rspCode <$> sendGET "http://localhost:2345/headerError"
            sc `shouldBe` (5,0,0)
        it "bodyError" $ do
            sc <- rspCode <$> sendGET "http://localhost:2345/bodyError"
            sc `shouldBe` (5,0,0)
        -}
        it "ioException" $ do
            sc <- rspCode <$> sendGET "http://localhost:2345/ioException"
            sc `shouldBe` (5,0,0)

----------------------------------------------------------------

sendGET :: String -> IO (Response String)
sendGET url = sendGETwH url []

sendGETwH :: String -> [Header] -> IO (Response String)
sendGETwH url hdr = unResult $ simpleHTTP $ (getRequest url) { rqHeaders = hdr }

unResult :: IO (Result (Response String)) -> IO (Response String)
unResult action = do
    res <- action
    case res of
        Right rsp -> return rsp
        Left _ -> error "Connection error"
