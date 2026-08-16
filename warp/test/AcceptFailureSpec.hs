{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module AcceptFailureSpec (spec) where

import Control.Concurrent
import Control.Exception
import Foreign.C.Error (eNFILE, errnoToIOError)
import Network.HTTP.Types (status200)
import Network.Socket
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp
import Test.Hspec

-- Run a server on an ephemeral port and report how runSettingsSocket ended.
--
-- The caller supplies the accept action, so a test can make accept() fail with
-- an errno of its choosing, and an action to run once the server is up.
runServerUntil
    :: (Socket -> IO (Socket, SockAddr))
    -> (Socket -> IO () -> IO ())
    -> IO (Either SomeException ())
runServerUntil accept' end = do
    shutdownSlot <- newEmptyMVar
    let application _ respond = respond (responseLBS status200 [] "ok")
        settings =
            setAccept accept' $
                setOnException (\_ _ -> return ()) $
                    setInstallShutdownHandler (putMVar shutdownSlot) defaultSettings

    bracket openListenSocket close $ \sock -> do
        outcome <- newEmptyMVar
        _ <- forkIO $ do
            r <- try (runSettingsSocket settings sock application)
            putMVar outcome r
        closeListenSocket <- takeMVar shutdownSlot
        threadDelay 200_000
        end sock closeListenSocket
        takeMVar outcome
  where
    openListenSocket = do
        sock <- socket AF_INET Stream defaultProtocol
        setSocketOption sock ReuseAddr 1
        bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
        listen sock 5
        return sock

spec :: Spec
spec = do
    describe "a listener that goes away" $ do
        it "ends the accept loop quietly when the socket is closed on purpose" $ do
            r <- runServerUntil accept $ \_ closeListenSocket -> closeListenSocket
            case r of
                Right () -> return ()
                Left e -> expectationFailure $ "graceful shutdown threw: " <> show e

        -- Without this, accept() failing leaves the caller with a plain (),
        -- which is exactly what a graceful shutdown returns. A server that can
        -- no longer accept is then indistinguishable from one that was asked
        -- to stop, so whatever supervises it never learns that it is dead.
        it "rethrows when accept fails for a reason nobody asked for" $ do
            let failingAccept _ = ioError (errnoToIOError "accept" eNFILE Nothing Nothing)
            r <- runServerUntil failingAccept $ \_ _ -> return ()
            case r of
                Left _ -> return ()
                Right () ->
                    expectationFailure
                        "runSettingsSocket returned normally after accept() failed, \
                        \so a lost listener looks just like a clean shutdown"
