{-# LANGUAGE OverloadedStrings #-}

module ConnectionExceptionSpec (spec) where

import Control.Concurrent (Chan, newChan, readChan, writeChan, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (link, withAsync)
import Control.Exception (Exception, SomeException, bracket, finally, fromException, throwIO, toException)
import Control.Monad (forM_, replicateM, void)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef')
import Data.Maybe (isJust)
import qualified Data.Streaming.Network as N
import Network.HTTP.Types (internalServerError500)
import Network.Socket (SockAddr (SockAddrInet), close, tupleToHostAddress)
import Network.Wai (remoteHost)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal (runSettingsConnectionMakerSecure)
import System.Timeout (timeout)
import Test.Hspec

import HTTP (responseStatus, sendGET)

-- Regression for https://github.com/yesodweb/wai/issues/1113. A connection
-- maker can fail before any Request exists, but Warp already owns its peer.
data ConnectionFailure = ConnectionFailure Int deriving (Show)
instance Exception ConnectionFailure

type Observation = (Int, Maybe SockAddr)

spec :: Spec
spec = describe "connection exception peer" $ do
    it "preserves the legacy observer when no peer observer is installed" $ do
        events <- newChan
        makers <- newChan
        let settings = setOnException (\request -> record events (remoteHost <$> request)) defaultSettings
        withAsync (runSettingsConnectionMakerSecure settings (readChan makers) unusedApplication) $ \server -> do
            link server
            writeChan makers (throwIO (ConnectionFailure 1), peer 100)
            timeout 2000000 (readChan events) `shouldReturn` Just (1, Nothing)

    it "gets the current legacy observer as the default connection observer" $ do
        events <- newChan
        let settings = setOnException (\request -> record events (remoteHost <$> request)) defaultSettings
        getOnConnectionException settings (peer 110) (toException (ConnectionFailure 2))
        timeout 2000000 (readChan events) `shouldReturn` Just (2, Nothing)

    it "uses only the connection observer regardless of setter order" $
        forM_ [False, True] $ \legacyLast -> do
            calls <- newIORef ([] :: [String])
            let legacy _ _ = modifyIORef' calls (++ ["legacy"])
                connection _ _ = modifyIORef' calls (++ ["connection"])
                settings = if legacyLast
                    then setOnException legacy $ setOnConnectionException connection defaultSettings
                    else setOnConnectionException connection $ setOnException legacy defaultSettings
            getOnConnectionException settings (peer 120) (toException (ConnectionFailure 3))
            readIORef calls `shouldReturn` ["connection"]

    it "keeps accept failures on the legacy observer because no peer was obtained" $ do
        calls <- newIORef ([] :: [(String, Bool)])
        let legacy request _ = modifyIORef' calls (++ [("legacy", isJust request)])
            connection _ _ = modifyIORef' calls (++ [("connection", False)])
            settings = setOnException legacy $ setOnConnectionException connection defaultSettings
        runSettingsConnectionMakerSecure settings (ioError (userError "accept failed")) unusedApplication
        readIORef calls `shouldReturn` [("legacy", False)]

    it "keeps application exceptions on the legacy observer with their request" $
        bracket (N.bindRandomPortTCP "127.0.0.1") (close . snd) $ \(port, listener) -> do
            events <- newChan
            connectionCalled <- newIORef False
            ready <- newEmptyMVar
            let legacy request exception = writeChan events (isJust request, show exception)
                connection _ _ = writeIORef connectionCalled True
                settings = setBeforeMainLoop (putMVar ready ())
                    $ setOnException legacy
                    $ setOnConnectionException connection defaultSettings
                application _ _ = throwIO (ConnectionFailure 4)
            withAsync (runSettingsSocket settings listener application) $ \server -> do
                link server
                timeout 2000000 (takeMVar ready) `shouldReturn` Just ()
                response <- sendGET ("http://127.0.0.1:" ++ show port ++ "/")
                responseStatus response `shouldBe` internalServerError500
                timeout 2000000 (readChan events) `shouldReturn` Just (True, "ConnectionFailure 4")
                readIORef connectionCalled `shouldReturn` False

    it "reports each sequential connection maker's own peer" $ do
        events <- newChan
        makers <- newChan
        let settings = observePeer (record events) defaultSettings
        withAsync (runSettingsConnectionMakerSecure settings (readChan makers) unusedApplication) $ \server -> do
            link server
            forM_ [(5, 130), (6, 140)] $ \(failureId, peerId) -> do
                writeChan makers (throwIO (ConnectionFailure failureId), peer peerId)
                timeout 2000000 (readChan events) `shouldReturn` Just (failureId, Just (peer peerId))

    it "reports peers when overlapping connection makers fail in reverse order" $ do
        events <- newChan
        makers <- newChan
        started <- newChan
        first <- newEmptyMVar
        second <- newEmptyMVar
        let settings = observePeer (record events) defaultSettings
            maker i gate = writeChan started i >> takeMVar gate >> throwIO (ConnectionFailure i)
        withAsync (runSettingsConnectionMakerSecure settings (readChan makers) unusedApplication) $ \server -> do
            link server
            writeChan makers (maker 7 first, peer 150)
            writeChan makers (maker 8 second, peer 160)
            -- Release both workers even when a readiness assertion fails.
            let release = forM_ [first, second] $ \gate -> void (tryPutMVar gate ())
            flip finally release $ do
                ready <- timeout 2000000 (replicateM 2 (readChan started))
                fmap length ready `shouldBe` Just 2
                putMVar second ()
                secondEvent <- timeout 2000000 (readChan events)
                putMVar first ()
                firstEvent <- timeout 2000000 (readChan events)
                (secondEvent, firstEvent) `shouldBe` (Just (8, Just (peer 160)), Just (7, Just (peer 150)))
  where
    unusedApplication _ _ = fail "connection maker must fail before the application"

-- Install the peer observer used by the regression assertions. The original
-- failing commit had to infer peers from Maybe Request.
observePeer :: (Maybe SockAddr -> SomeException -> IO ()) -> Settings -> Settings
observePeer report = setOnConnectionException (report . Just)

record :: Chan Observation -> Maybe SockAddr -> SomeException -> IO ()
record events address exception = case fromException exception of
    Just (ConnectionFailure i) -> writeChan events (i, address)
    Nothing -> throwIO exception

peer :: Int -> SockAddr
peer i = SockAddrInet (fromIntegral (40000 + i)) (tupleToHostAddress (127, 0, 0, fromIntegral i))
