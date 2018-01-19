{-# LANGUAGE OverloadedStrings, CPP #-}

module ExceptionSpec (main, spec) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Monad
import Network.HTTP.Types hiding (Header)
import Network.Wai hiding (Response, responseStatus)
import Network.Wai.Internal (Request(..))
import Network.Wai.Handler.Warp
import Test.Hspec
import Control.Exception
import qualified Data.Streaming.Network as N
import Control.Concurrent.Async (withAsync)
import Network.Socket (close)

import HTTP

main :: IO ()
main = hspec spec

withTestServer :: (Int -> IO a) -> IO a
withTestServer inner = bracket
    (N.bindRandomPortTCP "127.0.0.1")
    (close . snd)
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
            sc <- responseStatus <$> sendGET "http://127.0.0.1:2345/statusError"
            sc `shouldBe` internalServerError500
        it "headersError" $ do
            sc <- responseStatus <$> sendGET "http://127.0.0.1:2345/headersError"
            sc `shouldBe` internalServerError500
        it "headerError" $ do
            sc <- responseStatus <$> sendGET "http://127.0.0.1:2345/headerError"
            sc `shouldBe` internalServerError500
        it "bodyError" $ do
            sc <- responseStatus <$> sendGET "http://127.0.0.1:2345/bodyError"
            sc `shouldBe` internalServerError500
        -}
        it "ioException" $ withTestServer $ \prt -> do
            sc <- responseStatus <$> sendGET ("http://127.0.0.1:" ++ show prt ++ "/ioException")
            sc `shouldBe` internalServerError500
