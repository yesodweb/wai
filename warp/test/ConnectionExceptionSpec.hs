module ConnectionExceptionSpec (spec) where

import Control.Concurrent (Chan, newChan, readChan, writeChan, newEmptyMVar, putMVar, takeMVar, tryPutMVar)
import Control.Concurrent.Async (link, withAsync)
import Control.Exception (Exception, SomeException, finally, fromException, throwIO)
import Control.Monad (forM_, replicateM, void)
import Network.Socket (SockAddr (SockAddrInet), tupleToHostAddress)
import Network.Wai (remoteHost)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal (runSettingsConnectionMakerSecure)
import System.Timeout (timeout)
import Test.Hspec

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
            writeChan makers (throwIO (ConnectionFailure 1), peer 1)
            timeout 2000000 (readChan events) `shouldReturn` Just (1, Nothing)

    it "reports each sequential connection maker's own peer" $ do
        events <- newChan
        makers <- newChan
        let settings = observePeer (record events) defaultSettings
        withAsync (runSettingsConnectionMakerSecure settings (readChan makers) unusedApplication) $ \server -> do
            link server
            forM_ [1, 2] $ \i -> do
                writeChan makers (throwIO (ConnectionFailure i), peer i)
                timeout 2000000 (readChan events) `shouldReturn` Just (i, Just (peer i))

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
            writeChan makers (maker 1 first, peer 1)
            writeChan makers (maker 2 second, peer 2)
            -- Release both workers even when a readiness assertion fails.
            let release = forM_ [first, second] $ \gate -> void (tryPutMVar gate ())
            flip finally release $ do
                ready <- timeout 2000000 (replicateM 2 (readChan started))
                fmap length ready `shouldBe` Just 2
                putMVar second ()
                secondEvent <- timeout 2000000 (readChan events)
                putMVar first ()
                firstEvent <- timeout 2000000 (readChan events)
                (secondEvent, firstEvent) `shouldBe` (Just (2, Just (peer 2)), Just (1, Just (peer 1)))
  where
    unusedApplication _ _ = fail "connection maker must fail before the application"

-- The existing observer is the only public exception context available in
-- the red commit. The fix replaces this adapter with the peer-aware API;
-- the peer assertions above remain unchanged.
observePeer :: (Maybe SockAddr -> SomeException -> IO ()) -> Settings -> Settings
observePeer report = setOnException (\request -> report (remoteHost <$> request))

record :: Chan Observation -> Maybe SockAddr -> SomeException -> IO ()
record events address exception = case fromException exception of
    Just (ConnectionFailure i) -> writeChan events (i, address)
    Nothing -> throwIO exception

peer :: Int -> SockAddr
peer i = SockAddrInet (fromIntegral (40000 + i)) (tupleToHostAddress (127, 0, 0, fromIntegral i))
