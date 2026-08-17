{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module GracefulShutdownSpec (spec) where

import Control.Concurrent
import Control.Concurrent.Async
import Control.Exception (bracket)
import Control.Monad (void)
import Data.IORef
import Foreign.C.Error (eBADF, errnoToIOError)
import Network.HTTP.Client
import Network.HTTP.Types (ok200, status200)
import Network.Socket
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "graceful shutdown" $ do
    it "waits for a connection accepted just before it stopped accepting" $ do
        -- The window is between accepting a connection and the thread
        -- serving it being scheduled. Delaying the thread makes it wide
        -- enough to test; in a running server it is however long the RTS
        -- takes to get to the new thread.
        accepted <- newIORef (0 :: Int)
        closed <- newIORef (0 :: Int)

        let slowFork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ()
            slowFork act = void $ forkIOWithUnmask $ \unmask -> do
                threadDelay 200_000
                act unmask

            -- Take one connection, then stop accepting, with the error a
            -- closed listening socket actually gives.  Closing it for real
            -- would mean closing a descriptor the accept loop is parked on,
            -- which the IO manager does not survive cleanly.
            acceptOnlyOne sock = do
                taken <- atomicModifyIORef' accepted $ \n -> (n + 1, n)
                if taken == 0
                    then accept sock
                    else ioError (errnoToIOError "accept" eBADF Nothing Nothing)

            settings =
                setFork slowFork $
                    setAccept acceptOnlyOne $
                        setOnClose (\_ -> atomicModifyIORef' closed $ \n -> (n + 1, ())) $
                            setGracefulShutdownTimeout (Just 5) $
                                setOnException (\_ _ -> pure ()) defaultSettings

            app _ respond = respond $ responseLBS status200 [("Content-Length", "0")] ""

        bracket openFreePort (close . snd) $ \(testPort, sock) -> do
            -- Connect before the server exists. openFreePort has already put
            -- the socket in listen state, so this lands in its accept queue
            -- in the kernel and stays there: closing the client end sends a
            -- FIN but does not take it off the queue, and accept() still
            -- hands it over. Queueing it up front is what makes the accept
            -- loop's first accept() return immediately, rather than racing a
            -- client connecting alongside it, which on a loaded machine it
            -- can lose.
            bracket (openConnection testPort) close $ \_ -> pure ()

            withAsync (runSettingsSocket settings sock app) $ \server -> do
                timeout 30_000_000 (wait server)
                    >>= maybe (expectationFailure "Timeout waiting for server shutdown") pure
                -- Returning is what lets the process exit, so a connection
                -- still open here is one the client never hears back on.
                connectionsClosed <- readIORef closed
                connectionsClosed `shouldBe` 1

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
                response <- sendRequest
                responseStatus response `shouldBe` ok200
                lookup "Connection" (responseHeaders response) `shouldBe` Just "close"
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
    openConnection testPort = do
        client <- socket AF_INET Stream defaultProtocol
        connect client $
            SockAddrInet (fromIntegral testPort) (tupleToHostAddress (127, 0, 0, 1))
        pure client
    -- set number of clients to the number of keep-alive connections
    numClients = managerConnCount defaultManagerSettings
    connectionRefused = \case
        (HttpExceptionRequest _ (ConnectionFailure _)) -> True
        _ -> False