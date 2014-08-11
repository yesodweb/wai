{-# LANGUAGE OverloadedStrings #-}

module ExceptionSpec (main, spec) where

import Control.Applicative
import Control.Monad
import Network.HTTP
import Network.Stream
import Network.HTTP.Types hiding (Header)
import Network.Wai hiding (Response)
import Network.Wai.Internal (Request(..))
import Network.Wai.Handler.Warp
import Test.Hspec
import Control.Exception
import qualified Data.Streaming.Network as N
import Control.Concurrent.Async (withAsync)
import Network.Socket (sClose)

main :: IO ()
main = hspec spec

withTestServer :: (Int -> IO a) -> IO a
withTestServer inner = bracket
    (N.bindRandomPortTCP "127.0.0.1")
    (sClose . snd)
    $ \(prt, lsocket) -> do
        withAsync (runSettingsSocket defaultSettings lsocket testApp)
            $ \_ -> inner prt

testApp :: Application
testApp (Network.Wai.Internal.Request {pathInfo = [x]}) f
    | x == "statusError" =
        f $ responseLBS undefined [] "foo"
    | x == "headersError" =
        f $ responseLBS ok200 undefined "foo"
    | x == "headerError" =
        f $ responseLBS ok200 [undefined] "foo"
    | x == "bodyError" =
        f $ responseLBS ok200 [] undefined
    | x == "ioException" = do
        void $ fail "ioException"
        f $ responseLBS ok200 [] "foo"
testApp _ f =
        f $ responseLBS ok200 [] "foo"

spec :: Spec
spec = describe "responds even if there is an exception" $ do
        {- Disabling these tests. We can consider forcing evaluation in Warp.
        it "statusError" $ do
            sc <- rspCode <$> sendGET "http://127.0.0.1:2345/statusError"
            sc `shouldBe` (5,0,0)
        it "headersError" $ do
            sc <- rspCode <$> sendGET "http://127.0.0.1:2345/headersError"
            sc `shouldBe` (5,0,0)
        it "headerError" $ do
            sc <- rspCode <$> sendGET "http://127.0.0.1:2345/headerError"
            sc `shouldBe` (5,0,0)
        it "bodyError" $ do
            sc <- rspCode <$> sendGET "http://127.0.0.1:2345/bodyError"
            sc `shouldBe` (5,0,0)
        -}
        it "ioException" $ withTestServer $ \prt -> do
            sc <- rspCode <$> sendGET (concat $ ["http://127.0.0.1:", show prt, "/ioException"])
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
