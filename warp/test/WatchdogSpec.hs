{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module WatchdogSpec (spec) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import qualified Control.Exception as E
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Control.Monad (void)
import Network.HTTP.Types
import qualified Network.HTTP2.Client as C
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import Network.Wai
import Network.Wai.Handler.Warp
import System.TimeManager (TimeoutThread (..))
import System.Timeout (timeout)
import Test.Hspec

import RunSpec (withApp)

-- All tests run with a timeout of one second.
settings :: Settings
settings = setTimeout 1 defaultSettings

spec :: Spec
spec = describe "watchdog" $ do
    describe "HTTP/1.1" $ do
        it "closes an idle connection" $
            withApp settings okApp $ \port -> withSock port $ \s -> do
                mbs <- timeout 2500000 $ recvAll s
                mbs `shouldBe` Just ""

        it "does not limit a slow application" $
            withApp settings (slowApp 1500000) $ \port -> withSock port $ \s -> do
                sendAll s "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
                bs <- recvUntil s "slow"
                bs `shouldSatisfy` S.isPrefixOf "HTTP/1.1 200"

        it "does not limit a streaming application between chunks" $
            withApp settings (streamApp 1500000) $ \port -> withSock port $ \s -> do
                sendAll s "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
                bs <- recvUntil s "second"
                bs `shouldSatisfy` S.isInfixOf "first"

        it "times out a stalled request body" $ do
            result <- newEmptyMVar
            let app req respond = do
                    r <- E.try $ consume $ getRequestBodyChunk req
                    putMVar result $ either (Just . show) (const Nothing) r
                    either E.throwIO (const $ respond $ responseLBS status200 [] "") $
                        (r :: Either E.SomeException ByteString)
            withApp settings app $ \port -> withSock port $ \s -> do
                sendAll s "POST / HTTP/1.1\r\nHost: localhost\r\ncontent-length: 100\r\n\r\n"
                sendAll s "0123456789"
                r <- takeMVar result
                r `shouldBe` Just (show TimeoutThread)
                mbs <- timeout 1000000 $ recvAll s
                mbs `shouldBe` Just ""

        it "closes a connection whose peer does not read" $ do
            closed <- newEmptyMVar
            let settings' = setOnClose (\_ -> void $ tryPutMVar closed ()) settings
                big = L.replicate (64 * 1024 * 1024) 88
                app _ respond = respond $ responseLBS status200 [] big
            withApp settings' app $ \port -> withSock port $ \s -> do
                sendAll s "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
                mc <- timeout 2500000 $ takeMVar closed
                mc `shouldBe` Just ()

    describe "HTTP/2" $ do
        it "closes an idle connection" $ do
            pendingWith
                "warp hands an HTTP/2 connection to the http2 library, which \
                \does not supervise it yet: see Warp.HTTP2.http2"
            withApp settings okApp $ \port -> withSock port $ \s -> do
                sendAll s "PRI * HTTP/2.0\r\n\r\nSM\r\n\r\n"
                sendAll s emptySettingsFrame
                mbs <- timeout 2500000 $ recvAll s
                fmap S.null mbs `shouldBe` Just False

        it "does not limit a slow application" $
            withApp settings (slowApp 1500000) $ \port -> do
                (st, body) <- h2get port
                st `shouldBe` status200
                body `shouldBe` "slow"

        it "does not limit a streaming application between chunks" $
            withApp settings (streamApp 1500000) $ \port -> do
                (st, body) <- h2get port
                st `shouldBe` status200
                body `shouldBe` "first second"

----------------------------------------------------------------

okApp :: Application
okApp _ respond = respond $ responseLBS status200 [] "ok"

slowApp :: Int -> Application
slowApp us _ respond = do
    threadDelay us
    respond $ responseLBS status200 [] "slow"

streamApp :: Int -> Application
streamApp us _ respond = respond $ responseStream status200 [] $ \write flush -> do
    write (byteString "first ") >> flush
    threadDelay us
    write (byteString "second") >> flush

consume :: IO ByteString -> IO ByteString
consume rbody = S.concat <$> loop
  where
    loop = do
        bs <- rbody
        if S.null bs then return [] else (bs :) <$> loop

----------------------------------------------------------------

withSock :: Int -> (Socket -> IO a) -> IO a
withSock port body = do
    let hints = defaultHints{addrSocketType = Stream}
    addr : _ <- getAddrInfo (Just hints) (Just "127.0.0.1") (Just $ show port)
    E.bracket (openSocket addr) close $ \s -> do
        connect s $ addrAddress addr
        body s

-- | Receiving until EOF. Returning what came before the EOF, if
--   anything, with an empty 'ByteString' meaning nothing.
recvAll :: Socket -> IO ByteString
recvAll s = S.concat <$> loop
  where
    -- A peer which resets rather than closes is an EOF for our purposes.
    -- Only an 'E.IOException': the callers wrap this in 'timeout', which
    -- ends it by throwing, and catching that would turn "the server never
    -- closed the connection" into a pass.
    loop = do
        bs <- recv s 4096 `E.catch` \(_ :: E.IOException) -> return ""
        if S.null bs then return [] else (bs :) <$> loop

recvUntil :: Socket -> ByteString -> IO ByteString
recvUntil s needle = loop ""
  where
    loop acc
        | needle `S.isInfixOf` acc = return acc
        | otherwise = do
            bs <- recv s 4096
            if S.null bs then return acc else loop (acc <> bs)

-- | A SETTINGS frame with no parameters.
emptySettingsFrame :: ByteString
emptySettingsFrame = S.pack [0, 0, 0, 4, 0, 0, 0, 0, 0]

h2get :: Int -> IO (Status, ByteString)
h2get port = withSock port $ \s ->
    E.bracket (C.allocSimpleConfig s 4096) C.freeSimpleConfig $ \conf ->
        C.run cliconf conf $ \sendRequest _aux -> do
            let req = C.requestNoBody methodGet "/" []
            sendRequest req $ \rsp -> do
                body <- consume $ C.getResponseBodyChunk rsp
                return (maybe status500 id $ C.responseStatus rsp, body)
  where
    cliconf = C.defaultClientConfig{C.authority = S8.unpack "127.0.0.1"}
