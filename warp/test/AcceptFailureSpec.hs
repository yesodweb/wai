{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module AcceptFailureSpec (spec) where

import Control.Concurrent
import Control.Exception
import qualified Data.ByteString.Lazy as BL
import Data.IORef
import Foreign.C.Error (
    Errno (..),
    eNETDOWN,
    eNFILE,
    eOPNOTSUPP,
    errnoToIOError,
 )
import GHC.IO.Exception (IOException (..))
import HTTP (responseBody, sendGET)
import Network.HTTP.Types (status200)
import Network.Socket
import Network.Wai (responseLBS)
import Network.Wai.Handler.Warp
import System.Timeout (timeout)
import Test.Hspec

-- Did this come out of accept() failing with this errno?
--
-- Worth checking rather than taking any exception: a test that accepts
-- whatever it is given would still pass if the accept loop started throwing
-- something else entirely.
failedWith :: Errno -> SomeException -> Bool
failedWith (Errno wanted) e = case fromException e of
    Just ioe -> ioe_errno ioe == Just wanted
    Nothing -> False

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
                Left e
                    | failedWith eNFILE e -> return ()
                    | otherwise ->
                        expectationFailure $ "expected the ENFILE from accept(), got: " <> show e
                Right () ->
                    expectationFailure
                        "runSettingsSocket returned normally after accept() failed, \
                        \so a lost listener looks just like a clean shutdown"

        -- Not a reason to stop: the socket is fine and the connection that
        -- failed is already off the queue, so serving continues.
        it "keeps accepting when one queued connection fails" $ do
            failuresLeft <- newIORef (3 :: Int)
            let flakyAccept sock = do
                    left <- atomicModifyIORef' failuresLeft $ \n -> (max 0 (n - 1), n)
                    if left > 0
                        then ioError (errnoToIOError "accept" eNETDOWN Nothing Nothing)
                        else accept sock
            served <- newIORef Nothing
            r <- runServerUntil flakyAccept $ \sock closeListenSocket -> do
                port <- socketPort sock
                body <-
                    try $
                        responseBody
                            <$> sendGET ("http://127.0.0.1:" ++ show port ++ "/")
                writeIORef served (Just (body :: Either SomeException BL.ByteString))
                closeListenSocket
            outcome <- readIORef served
            case outcome of
                Just (Right body) -> body `shouldBe` "ok"
                Just (Left e) ->
                    expectationFailure $
                        "the server stopped accepting after a queued connection \
                        \failed, so the next request went unanswered: "
                            <> show e
                Nothing -> expectationFailure "the request was never made"
            case r of
                Right () -> return ()
                Left e -> expectationFailure $ "graceful shutdown threw: " <> show e

        -- eOPNOTSUPP is a queued-connection error in accept(2) and also what a
        -- listening socket that is not SOCK_STREAM answers. The two are
        -- indistinguishable here, and retrying the second spins forever, so it
        -- is thrown rather than retried.
        it "rethrows rather than spinning when the socket cannot accept" $ do
            let unsupportedAccept _ =
                    ioError (errnoToIOError "accept" eOPNOTSUPP Nothing Nothing)
            r <- timeout 5_000_000 $ runServerUntil unsupportedAccept $ \_ _ -> return ()
            case r of
                Nothing -> expectationFailure "the accept loop spun instead of giving up"
                Just (Left e)
                    | failedWith eOPNOTSUPP e -> return ()
                    | otherwise ->
                        expectationFailure $ "expected the EOPNOTSUPP from accept(), got: " <> show e
                Just (Right ()) ->
                    expectationFailure
                        "runSettingsSocket returned normally on a socket that cannot accept"
