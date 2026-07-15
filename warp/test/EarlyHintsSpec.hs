{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module EarlyHintsSpec (spec) where

import Test.Hspec

#define HAS_EARLY_HINTS_SUPPORT (MIN_VERSION_http_semantics(0,4,1) && MIN_VERSION_http2(5,4,2))

#if HAS_EARLY_HINTS_SUPPORT
import Control.Exception (bracket)
import Data.IORef
import Network.HPACK (TokenHeaderTable, getFieldValue)
import Network.HPACK.Token (toToken)
import Network.HTTP.Types (Status, methodGet, ok200, status200, status404)
import qualified Network.HTTP2.Client as C
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp (Port, testWithApplication)

spec :: Spec
spec = describe "HTTP/2 Early Hints" $
    it "delivers a WAI app's 103 Early Hints to the client before the final response (h2c)" $
        testWithApplication (pure app) $ \port -> do
            hintsRef <- newIORef []
            earlyHintsClient port hintsRef >>= (`shouldBe` Just ok200)
            hints <- readIORef hintsRef
            map (getFieldValue (toToken "link") . snd) hints
                `shouldBe` [ Just "</style.css>; rel=preload; as=style"
                           , Just "</app.js>; rel=preload; as=script"
                           ]

-- | A WAI app that emits two Early Hints sections, then the final response.
app :: Application
app req respond
    | pathInfo req == ["early"] = do
        requestSendEarlyHints req [("link", "</style.css>; rel=preload; as=style")]
        requestSendEarlyHints req [("link", "</app.js>; rel=preload; as=script")]
        respond $ responseLBS status200 [("content-type", "text/plain")] "Hello"
    | otherwise = respond $ responseLBS status404 [] ""

-- | Drive Warp over h2c with the HTTP/2 client, recording each 103 Early Hints
--   section via the client's informational handler, and return the final status.
earlyHintsClient :: Port -> IORef [TokenHeaderTable] -> IO (Maybe Status)
earlyHintsClient port hintsRef = withTCP "127.0.0.1" port $ \sock ->
    bracket (C.allocSimpleConfig sock 4096) C.freeSimpleConfig $ \conf ->
        C.run cliconf (conf{C.confOnInformational = onInformational}) $ \sendRequest _aux ->
            sendRequest (C.requestNoBody methodGet "/early" []) (return . C.responseStatus)
  where
    cliconf = C.defaultClientConfig{C.authority = "127.0.0.1"}
    onInformational _streamId tbl = modifyIORef' hintsRef (++ [tbl])

-- | Connect to a TCP server, run an action, and close the socket afterwards.
withTCP :: HostName -> Port -> (Socket -> IO a) -> IO a
withTCP host port = bracket open close
  where
    open = do
        addr : _ <- getAddrInfo (Just defaultHints{addrSocketType = Stream}) (Just host) (Just (show port))
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        return sock
#else
spec :: Spec
spec = describe "HTTP/2 Early Hints" $
    it "delivers a WAI app's 103 Early Hints to the client before the final response (h2c)" $
        pendingWith "requires http2 >= 5.4.2 and http-semantics >= 0.4.1"
#endif
