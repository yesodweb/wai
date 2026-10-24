{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (newChan, readChan, writeChan, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Concurrent.Async (link, mapConcurrently, withAsync)
import Control.Exception (SomeException, bracket, fromException, throwIO)
import Control.Monad (forM_, replicateM, when)
import Data.Word (Word8)
import qualified Network.Socket as S
import qualified Data.ByteString as BS
import qualified Network.Socket.ByteString as B
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as TLS
import GHC.Clock (getMonotonicTime)
import System.Info (os)
import System.Timeout (timeout)
import Test.Hspec

main :: IO ()
main = hspec $ do
    describe "the watchdog of a connection being established" $
        it "gives the whole of it one timeout, not one per step" $
            stallHalfWayIn

    describe "pre-TLS exception peer (#1113)" $ do
        it "attributes sequential plaintext and premature-close failures to their sockets" $
            exercise [1, 1] False
        it "attributes concurrent plaintext and premature-close failures to their sockets" $
            exercise [1, 1] True

    -- Linux provides the whole 127/8 loopback range without interface aliases.
    -- Keep the original two-IP regression there; other platforms still check
    -- exact socket addresses (including source ports) above. The maker tests
    -- additionally check distinct IPs without binding sockets on every platform.
        when (os == "linux") $ do
            it "attributes sequential failures from distinct loopback IPs" $
                exercise [1, 2] False
            it "attributes concurrent failures from distinct loopback IPs" $
                exercise [1, 2] True

-- | Warp starts the watchdog of a connection before it runs the connection
-- maker, so what the maker does is watched too.  A client which dawdles
-- before the handshake and then stalls used to be given the timeout twice
-- over: once by the timeout around the first read, and again by the one
-- around the handshake, each starting from scratch.
stallHalfWayIn :: IO ()
stallHalfWayIn = S.withSocketsDo $
    bracket (S.socket S.AF_INET S.Stream S.defaultProtocol) S.close $ \listener -> do
        S.bind listener (S.SockAddrInet 0 (S.tupleToHostAddress (127, 0, 0, 1)))
        S.listen listener 10
        port <- S.socketPort listener
        ready <- newEmptyMVar
        let settings =
                Warp.setTimeout 1 $
                    Warp.setBeforeMainLoop (putMVar ready ()) $
                        Warp.setOnConnectionException (\_ _ -> pure ()) Warp.defaultSettings
            application _ _ = fail "connection must fail before the application"
            tls = TLS.tlsSettings "certificate.pem" "key.pem"
        withAsync (TLS.runTLSSocket tls settings listener application) $ \server -> do
            link server
            timeout 2000000 (takeMVar ready) `shouldReturn` Just ()
            bracket (S.socket S.AF_INET S.Stream S.defaultProtocol) S.close $ \client -> do
                started <- getMonotonicTime
                S.connect client (S.SockAddrInet port (S.tupleToHostAddress (127, 0, 0, 1)))
                -- Late, but inside the timeout, and then nothing: the first
                -- byte of a TLS record and no record to go with it.
                threadDelay 700000
                B.sendAll client (BS.pack [0x16])
                closed <- timeout 3000000 (drain client)
                ended <- getMonotonicTime
                closed `shouldBe` Just ()
                (ended - started) `shouldSatisfy` (< 1.5)

-- These regressions deliberately leave Warp's accept and fork hooks alone.
-- Keep both peers and failure kinds when changing the exception observer.
exercise :: [Word8] -> Bool -> IO ()
exercise hosts concurrent = S.withSocketsDo $
    bracket (S.socket S.AF_INET S.Stream S.defaultProtocol) S.close $ \listener -> do
        S.bind listener (S.SockAddrInet 0 (S.tupleToHostAddress (127, 0, 0, 1)))
        S.listen listener 10
        port <- S.socketPort listener
        events <- newChan
        ready <- newEmptyMVar
        let report address exception = case fromException exception of
                Just tlsException -> writeChan events (address, show (tlsException :: TLS.WarpTLSException))
                Nothing -> throwIO exception
            settings = Warp.setBeforeMainLoop (putMVar ready ()) $ observePeer report Warp.defaultSettings
            application _ _ = fail "connection must fail before the application"
            tls = TLS.tlsSettings "certificate.pem" "key.pem"
            clients = [(host, plaintext) | host <- hosts, plaintext <- [True, False]]
            send (host, plaintext) = bracket (S.socket S.AF_INET S.Stream S.defaultProtocol) S.close $ \client -> do
                S.bind client (S.SockAddrInet 0 (S.tupleToHostAddress (127, 0, 0, host)))
                S.connect client (S.SockAddrInet port (S.tupleToHostAddress (127, 0, 0, 1)))
                own <- S.getSocketName client
                if plaintext
                    then do
                        B.sendAll client "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
                        -- Let WarpTLS send its denial before closing the client;
                        -- an early close could cause a different send exception.
                        timeout 2000000 (drain client) `shouldReturn` Just ()
                    else S.shutdown client S.ShutdownSend
                pure (Just own, if plaintext then "InsecureConnectionDenied" else "ClientClosedConnectionPrematurely")
        withAsync (TLS.runTLSSocket tls settings listener application) $ \server -> do
            link server
            timeout 2000000 (takeMVar ready) `shouldReturn` Just ()
            if concurrent
                then do
                    expected <- mapConcurrently send clients
                    actual <- timeout 2000000 (replicateM (length clients) (readChan events))
                    fmap length actual `shouldBe` Just (length clients)
                    maybe (expectationFailure "missing TLS observations") (`shouldMatchList` expected) actual
                else forM_ clients $ \client -> do
                    expected <- send client
                    timeout 2000000 (readChan events) `shouldReturn` Just expected

observePeer :: (Maybe S.SockAddr -> SomeException -> IO ()) -> Warp.Settings -> Warp.Settings
observePeer report = Warp.setOnConnectionException (report . Just)

drain :: S.Socket -> IO ()
drain client = do
    bytes <- B.recv client 4096
    if bytes == "" then pure () else drain client
