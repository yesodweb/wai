{-# LANGUAGE CPP #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module HTTP2Spec (spec) where

import Control.Concurrent (
    forkIO,
    newEmptyMVar,
    putMVar,
    readMVar,
    takeMVar,
    threadDelay,
 )
import Control.Concurrent.Async (wait, withAsync)
import Control.Exception (SomeException, bracket, throwIO, try)
import Control.Monad (join)
import Data.ByteString (ByteString)
import Data.IORef
import Network.HTTP.Types (Status, ok200, status200)
import qualified Network.HTTP2.Client as C
import Network.Socket
import Network.Wai
import Network.Wai.Handler.Warp (
    Port,
    defaultSettings,
    openFreePort,
    runSettingsSocket,
    setGracefulShutdownTimeout,
    setInstallShutdownHandler,
    setLogger,
    setOnException,
    withApplicationSettings,
 )
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "http2server" $ do
    it "reports nothing for a connection the client closes" $ do
        excs <- newIORef []
        let settings = setOnException (\_ e -> modifyIORef' excs (show e :)) defaultSettings
        withApplicationSettings settings (pure hello) $ \port -> do
            get port `shouldReturn` (Just ok200, "hello")
            -- The connection ends once the client has closed it.
            threadDelay 100000
            -- http2's ConnectionIsClosed used to be reported here.
            readIORef excs `shouldReturn` []
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
#if MIN_VERSION_http2(5,4,13)
    it "refuses new streams once stopping and finishes the open ones" $ do
        started <- newEmptyMVar
        release <- newEmptyMVar
        stopSlot <- newEmptyMVar
        stopped <- newEmptyMVar
        let settings =
                setInstallShutdownHandler (putMVar stopSlot) $
                    setGracefulShutdownTimeout (Just 5) $
                        setOnException (\_ _ -> pure ()) defaultSettings
            slow req respond
                | rawPathInfo req == "/slow" = do
                    putMVar started ()
                    takeMVar release
                    respond $ responseLBS status200 [] "slow"
                | otherwise = hello req respond
        bracket openFreePort (close . snd) $ \(port, sock) -> do
            _ <- forkIO $ runSettingsSocket settings sock slow >> putMVar stopped ()
            (r1, r2) <- withH2 port $ \sendRequest ->
                withAsync (sendRequest (C.requestNoBody "GET" "/slow" []) firstChunk) $ \a -> do
                    takeMVar started
                    -- Stop the server, as a signal handler would.
                    join $ readMVar stopSlot
                    -- GOAWAY, its PING and the second GOAWAY.
                    threadDelay 500_000
                    r2 <- try $ sendRequest (C.requestNoBody "GET" "/" []) firstChunk
                    putMVar release ()
                    r1 <- wait a
                    return (r1, r2 :: Either SomeException (Maybe Status, ByteString))
            r1 `shouldBe` (Just ok200, "slow")
            -- It used to be served: a busy connection went on taking streams.
            either (const True) (const False) r2 `shouldBe` True
            timeout 5_000_000 (takeMVar stopped) `shouldReturn` Just ()
#endif

hello :: Application
hello _ respond = respond $ responseLBS status200 [] "hello"

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
            sendRequest (C.requestNoBody "GET" "/" []) firstChunk
  where
    cliconf = C.defaultClientConfig{C.authority = "127.0.0.1"}

-- | The status and the first chunk of the body of a response.
firstChunk :: C.Response -> IO (Maybe Status, ByteString)
firstChunk rsp = do
    b <- C.getResponseBodyChunk rsp
    return (C.responseStatus rsp, b)

-- | Run an h2c client on one connection.
withH2
    :: Port
    -> ((forall r. C.Request -> (C.Response -> IO r) -> IO r) -> IO a)
    -> IO a
withH2 port client = withTCP "127.0.0.1" port $ \sock ->
    bracket (C.allocSimpleConfig sock 4096) C.freeSimpleConfig $ \conf ->
        C.run cliconf conf $ \sendRequest _aux -> client sendRequest
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
