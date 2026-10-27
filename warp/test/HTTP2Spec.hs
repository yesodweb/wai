{-# LANGUAGE OverloadedStrings #-}

module HTTP2Spec (spec) where

import Control.Exception (bracket, throwIO)
import Data.ByteString (ByteString)
import Data.IORef
import Network.HTTP.Types (Status, ok200, status200)
import qualified Network.HTTP2.Client as C
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp (
    Port,
    defaultSettings,
    setLogger,
    setOnException,
    withApplicationSettings,
 )
import Test.Hspec

spec :: Spec
spec = describe "http2server" $
    it "keeps the response of an application which throws after it" $ do
        excs <- newIORef (0 :: Int)
        logged <- newIORef []
        let settings =
                setLogger (\_ st _ -> modifyIORef' logged (st :)) $
                    setOnException (\_ _ -> modifyIORef' excs (+ 1)) defaultSettings
        withApplicationSettings settings (pure app) $ \port -> do
            get port `shouldReturn` (Just ok200, "hello")
            readIORef excs `shouldReturn` 1
            -- The second response used to go out and be logged, so the
            -- log said 500 where the client had been answered 200.
            readIORef logged `shouldReturn` [ok200]

-- | Answers, then throws.  The response has gone out by then, so
--   nothing can replace it.
app :: Application
app _ respond = do
    _ <- respond $ responseLBS status200 [] "hello"
    throwIO $ userError "after the response"

-- | The status and body of one h2c request.
get :: Port -> IO (Maybe Status, ByteString)
get port = withTCP "127.0.0.1" port $ \sock ->
    bracket (C.allocSimpleConfig sock 4096) C.freeSimpleConfig $ \conf ->
        C.run cliconf conf $ \sendRequest _aux ->
            sendRequest (C.requestNoBody "GET" "/" []) $ \rsp -> do
                body <- C.getResponseBodyChunk rsp
                return (C.responseStatus rsp, body)
  where
    cliconf = C.defaultClientConfig{C.authority = "127.0.0.1"}

-- | Connect to a TCP server, run an action, and close the socket afterwards.
withTCP :: HostName -> Port -> (Socket -> IO a) -> IO a
withTCP host port = bracket open close
  where
    open = do
        addr : _ <- getAddrInfo (Just defaultHints{addrSocketType = Stream}) (Just host) (Just (show port))
        sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
        connect sock (addrAddress addr)
        return sock
