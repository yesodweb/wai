{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module GracefulShutdownSpec (spec) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad (void)
import Data.Functor ((<&>))
import Network.HTTP.Client
import Network.HTTP.Types
import Network.Socket (close)
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "graceful shutdown" $
    it "serves the request in flight, then closes keep-alive connections and exits" $ do
        shutdownSignal <- newEmptyMVar
        allowResponse <- newEmptyMVar
        receivedRequests <- newQSemN 0
        allowSecondRequest <- newEmptyMVar

        let installShutdownHandler closeListenSocket =
                void . forkIO $ do
                    readMVar shutdownSignal
                    closeListenSocket

            settings =
                setInstallShutdownHandler installShutdownHandler defaultSettings

            app _ respond = do
                -- signal 1 received request
                signalQSemN receivedRequests 1
                -- block until signaled
                readMVar allowResponse
                respond $ responseLBS status200 [("Content-Length", "0")] ""

            client sendRequest = do
                -- first request should return OK
                sendRequest
                    >>= (responseStatus <&> (`shouldBe` ok200))
                        <> (lookup "Connection" . responseHeaders <&> (`shouldBe` Just "close"))
                -- wait with the second request
                void $ readMVar allowSecondRequest
                -- second request should end with connection refused
                sendRequest `shouldThrow` connectionRefused

        bracket openFreePort (close . snd) $ \(testPort, sock) ->
            withAsync (runSettingsSocket settings sock app) $ \server -> do
                manager <- newManager defaultManagerSettings
                request <- parseRequest ("http://127.0.0.1:" ++ show testPort)
                withAsync
                    -- start all clients
                    ( replicateConcurrently_ numClients $
                        client (httpNoBody request manager)
                    )
                    $ \clients -> do
                        -- wait for all clients to send requests
                        waitQSemN receivedRequests numClients
                        -- shutdown the server before serving requests
                        putMVar shutdownSignal ()
                        -- wait a little - otherwise some requests might not get
                        -- Connection: close response header
                        threadDelay 100_000
                        -- let requests be handled
                        putMVar allowResponse ()
                        -- server should exit
                        timeout 5_000_000 (wait server)
                            >>= maybe (expectationFailure "Timeout waiting for server shutdown") pure
                        -- let clients proceed with the second request
                        putMVar allowSecondRequest ()
                        -- wait for all clients and propagate any exceptions
                        wait clients
  where
    -- set number of clients to the number of keep-alive connections
    numClients = managerConnCount defaultManagerSettings
    connectionRefused = \case
        (HttpExceptionRequest _ (ConnectionFailure _)) -> True
        _ -> False
