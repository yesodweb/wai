{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module DisconnectSpec (spec) where

import Control.Concurrent.MVar (newEmptyMVar, putMVar, readMVar, takeMVar)
import Control.Exception (finally)
import Data.ByteString (ByteString)
import Data.Maybe (isNothing)
#if MIN_VERSION_network(3,1,2)
import Control.Exception (bracket)
import Data.Streaming.Network (getSocketTCP)
import Network.Socket (ShutdownCmd (ShutdownSend), close, shutdown)
import qualified Network.Socket.ByteString as Socket
#endif
import Network.HTTP.Types (status200, status503)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp
import RunSpec (msRead, msWrite, withApp, withMySocket)
import Test.Hspec

spec :: Spec
spec = describe "HTTP/1.1 disconnect monitoring" $ do
    it "is opt-in: no waiter when disabled" $ do
        observed <- newEmptyMVar
        let app req respond = do
                putMVar observed (isNothing (requestWaitForDisconnect req))
                respond (responseLBS status200 [] "ok")
        withApp defaultSettings app $ withMySocket $ \client -> do
            msWrite client requestBytes
            takeMVar observed `shouldReturn` True
            _ <- msRead client 1
            return ()

    it "can wait repeatedly without committing the final HTTP status" $ do
        observed <- newEmptyMVar
        let detector _socket = return ()
            app req respond = do
                case requestWaitForDisconnect req of
                    Nothing -> putMVar observed False
                    Just waitForDisconnect -> do
                        waitForDisconnect
                        waitForDisconnect
                        putMVar observed True
                respond (responseLBS status503 [] "upstream cancelled")
        withApp (setSocketDisconnectMonitor detector defaultSettings) app $ withMySocket $ \client -> do
            msWrite client requestBytes
            takeMVar observed `shouldReturn` True
            msRead client 12 `shouldReturn` "HTTP/1.1 503"

    it "stops the detector when the HTTP/1.1 connection ends" $ do
        started <- newEmptyMVar
        stopped <- newEmptyMVar
        never <- newEmptyMVar
        let detector _socket =
                (putMVar started () >> takeMVar never)
                    `finally` putMVar stopped ()
            app _req respond = do
                readMVar started
                respond (responseLBS status200 [] "ok")
        withApp (setSocketDisconnectMonitor detector defaultSettings) app $ withMySocket $ \client -> do
            msWrite client $ "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
            _ <- msRead client 4096
            takeMVar stopped

#if MIN_VERSION_network(3,1,2)
    it "the built-in TCP detector observes a receive EOF" $ do
        observed <- newEmptyMVar
        let app req respond = do
                case requestWaitForDisconnect req of
                    Nothing -> putMVar observed False
                    Just waitForDisconnect -> waitForDisconnect >> putMVar observed True
                respond (responseLBS status200 [] "ok")
        withApp (setSocketDisconnectMonitor waitForSocketDisconnect defaultSettings) app $ \port ->
            bracket (fst <$> getSocketTCP "127.0.0.1" port) close $ \client -> do
                Socket.sendAll client requestBytes
                -- Close only the client's sending side: the detectable event
                -- is TCP receive EOF, not proof the response cannot be read.
                shutdown client ShutdownSend
                takeMVar observed `shouldReturn` True
#endif

requestBytes :: ByteString
requestBytes = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
