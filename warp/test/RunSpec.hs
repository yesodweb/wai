{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunSpec (main, spec, withApp) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forM_, replicateM_)
import System.Timeout (timeout)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString, hPutStr, hGetSome)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.Conduit (($$), (=$))
import qualified Data.Conduit.List
import qualified Data.IORef as I
import Network (connectTo, PortID (PortNumber))
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO (hFlush, hClose)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Exception.Lifted (bracket, try, IOException, onException)
import Data.Streaming.Network (bindPortTCP)
import Network.Socket (sClose)
import qualified Network.HTTP as HTTP

main :: IO ()
main = hspec spec

type Counter = I.IORef (Either String Int)
type CounterApplication = Counter -> Application

incr :: MonadIO m => Counter -> m ()
incr icount = liftIO $ I.atomicModifyIORef icount $ \ecount ->
    ((case ecount of
        Left s -> Left s
        Right i -> Right $ i + 1), ())

err :: (MonadIO m, Show a) => Counter -> a -> m ()
err icount msg = liftIO $ I.writeIORef icount $ Left $ show msg

readBody :: CounterApplication
readBody icount req = do
    body <- requestBody req $$ Data.Conduit.List.consume
    case () of
        ()
            | pathInfo req == ["hello"] && L.fromChunks body /= "Hello"
                -> err icount ("Invalid hello" :: String, body)
            | requestMethod req == "GET" && L.fromChunks body /= ""
                -> err icount ("Invalid GET" :: String, body)
            | not $ requestMethod req `elem` ["GET", "POST"]
                -> err icount ("Invalid request method (readBody)" :: String, requestMethod req)
            | otherwise -> incr icount
    return $ responseLBS status200 [] "Read the body"

ignoreBody :: CounterApplication
ignoreBody icount req = do
    if (requestMethod req `elem` ["GET", "POST"])
        then incr icount
        else err icount ("Invalid request method" :: String, requestMethod req)
    return $ responseLBS status200 [] "Ignored the body"

doubleConnect :: CounterApplication
doubleConnect icount req = do
    _ <- requestBody req $$ Data.Conduit.List.consume
    _ <- requestBody req $$ Data.Conduit.List.consume
    incr icount
    return $ responseLBS status200 [] "double connect"

nextPort :: I.IORef Int
nextPort = unsafePerformIO $ I.newIORef 5000

getPort :: IO Int
getPort = do
    port <- I.atomicModifyIORef nextPort $ \p -> (p + 1, p)
    esocket <- try $ bindPortTCP port "*4"
    case esocket of
        Left (_ :: IOException) -> RunSpec.getPort
        Right socket -> do
            sClose socket
            return port

withApp :: Settings -> Application -> (Int -> IO a) -> IO a
withApp settings app f = do
    port <- RunSpec.getPort
    baton <- newEmptyMVar
    let settings' = setPort port
                  $ setBeforeMainLoop (putMVar baton ())
                    settings
    bracket
        (forkIO $ runSettings settings' app `onException` putMVar baton ())
        killThread
        (const $ takeMVar baton >> f port)

runTest :: Int -- ^ expected number of requests
        -> CounterApplication
        -> [ByteString] -- ^ chunks to send
        -> IO ()
runTest expected app chunks = do
    ref <- I.newIORef (Right 0)
    withApp defaultSettings (app ref) $ \port -> do
        handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
        forM_ chunks $ \chunk -> hPutStr handle chunk >> hFlush handle
        _ <- timeout 100000 $ replicateM_ expected $ hGetSome handle 4096
        res <- I.readIORef ref
        case res of
            Left s -> error s
            Right i -> i `shouldBe` expected

dummyApp :: Application
dummyApp _ = return $ responseLBS status200 [] "foo"

runTerminateTest :: InvalidRequest
                 -> ByteString
                 -> IO ()
runTerminateTest expected input = do
    ref <- I.newIORef Nothing
    let onExc _ = I.writeIORef ref . Just
    withApp (setOnException onExc defaultSettings) dummyApp $ \port -> do
        handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
        hPutStr handle input
        hFlush handle
        hClose handle
        threadDelay 1000
        res <- I.readIORef ref
        show res `shouldBe` show (Just expected)

singleGet :: ByteString
singleGet = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"

singlePostHello :: ByteString
singlePostHello = "POST /hello HTTP/1.1\r\nHost: localhost\r\nContent-length: 5\r\n\r\nHello"

spec :: Spec
spec = do
    describe "non-pipelining" $ do
        it "no body, read" $ runTest 5 readBody $ replicate 5 singleGet
        it "no body, ignore" $ runTest 5 ignoreBody $ replicate 5 singleGet
        it "has body, read" $ runTest 2 readBody
            [ singlePostHello
            , singleGet
            ]
        it "has body, ignore" $ runTest 2 ignoreBody
            [ singlePostHello
            , singleGet
            ]
    describe "pipelining" $ do
        it "no body, read" $ runTest 5 readBody [S.concat $ replicate 5 singleGet]
        it "no body, ignore" $ runTest 5 ignoreBody [S.concat $ replicate 5 singleGet]
        it "has body, read" $ runTest 2 readBody $ return $ S.concat
            [ singlePostHello
            , singleGet
            ]
        it "has body, ignore" $ runTest 2 ignoreBody $ return $ S.concat
            [ singlePostHello
            , singleGet
            ]
    describe "no hanging" $ do
        it "has body, read" $ runTest 1 readBody $ map S.singleton $ S.unpack singlePostHello
        it "double connect" $ runTest 1 doubleConnect [singlePostHello]

    describe "connection termination" $ do
--        it "ConnectionClosedByPeer" $ runTerminateTest ConnectionClosedByPeer "GET / HTTP/1.1\r\ncontent-length: 10\r\n\r\nhello"
        it "IncompleteHeaders" $ runTerminateTest IncompleteHeaders "GET / HTTP/1.1\r\ncontent-length: 10\r\n"

    describe "special input" $ do
        it "multiline headers" $ do
            iheaders <- I.newIORef []
            let app req = do
                    liftIO $ I.writeIORef iheaders $ requestHeaders req
                    return $ responseLBS status200 [] ""
            withApp defaultSettings app $ \port -> do
                handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
                let input = S.concat
                        [ "GET / HTTP/1.1\r\nfoo:    bar\r\n baz\r\n\tbin\r\n\r\n"
                        ]
                hPutStr handle input
                hFlush handle
                hClose handle
                threadDelay 1000
                headers <- I.readIORef iheaders
                headers `shouldBe`
                    [ ("foo", "bar baz\tbin")
                    ]
        it "no space between colon and value" $ do
            iheaders <- I.newIORef []
            let app req = do
                    liftIO $ I.writeIORef iheaders $ requestHeaders req
                    return $ responseLBS status200 [] ""
            withApp defaultSettings app $ \port -> do
                handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
                let input = S.concat
                        [ "GET / HTTP/1.1\r\nfoo:bar\r\n\r\n"
                        ]
                hPutStr handle input
                hFlush handle
                hClose handle
                threadDelay 1000
                headers <- I.readIORef iheaders
                headers `shouldBe`
                    [ ("foo", "bar")
                    ]

    describe "chunked bodies" $ do
        it "works" $ do
            ifront <- I.newIORef id
            let app req = do
                    bss <- requestBody req $$ Data.Conduit.List.consume
                    liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                    return $ responseLBS status200 [] ""
            withApp defaultSettings app $ \port -> do
                handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
                let input = S.concat
                        [ "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"
                        , "c\r\nHello World\n\r\n3\r\nBye\r\n0\r\n\r\n"
                        , "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"
                        , "b\r\nHello World\r\n0\r\n\r\n"
                        ]
                hPutStr handle input
                hFlush handle
                hClose handle
                threadDelay 1000
                front <- I.readIORef ifront
                front [] `shouldBe`
                    [ "Hello World\nBye"
                    , "Hello World"
                    ]
        it "lots of chunks" $ do
            ifront <- I.newIORef id
            let app req = do
                    bss <- requestBody req $$ Data.Conduit.List.consume
                    liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                    return $ responseLBS status200 [] ""
            withApp defaultSettings app $ \port -> do
                handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
                let input = concat $ replicate 2 $
                        ["POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"] ++
                        (replicate 50 "5\r\n12345\r\n") ++
                        ["0\r\n\r\n"]
                mapM_ (\bs -> hPutStr handle bs >> hFlush handle) input
                hClose handle
                threadDelay 1000
                front <- I.readIORef ifront
                front [] `shouldBe` replicate 2 (S.concat $ replicate 50 "12345")
        it "in chunks" $ do
            ifront <- I.newIORef id
            let app req = do
                    bss <- requestBody req $$ Data.Conduit.List.consume
                    liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                    return $ responseLBS status200 [] ""
            withApp defaultSettings app $ \port -> do
                handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
                let input = S.concat
                        [ "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"
                        , "c\r\nHello World\n\r\n3\r\nBye\r\n0\r\n"
                        , "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"
                        , "b\r\nHello World\r\n0\r\n\r\n"
                        ]
                mapM_ (\bs -> hPutStr handle bs >> hFlush handle) $ map S.singleton $ S.unpack input
                hClose handle
                threadDelay 1000
                front <- I.readIORef ifront
                front [] `shouldBe`
                    [ "Hello World\nBye"
                    , "Hello World"
                    ]
        it "timeout in request body" $ do
            ifront <- I.newIORef id
            let app req = do
                    bss <- (requestBody req $$ Data.Conduit.List.consume) `onException`
                        liftIO (I.atomicModifyIORef ifront (\front -> (front . ("consume interrupted":), ())))
                    liftIO $ threadDelay 4000000 `onException`
                        I.atomicModifyIORef ifront (\front -> (front . ("threadDelay interrupted":), ()))
                    liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                    return $ responseLBS status200 [] ""
            withApp (setTimeout 1 defaultSettings) app $ \port -> do
                let bs1 = S.replicate 2048 88
                    bs2 = "This is short"
                    bs = S.append bs1 bs2
                handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
                hPutStr handle "POST / HTTP/1.1\r\n"
                hPutStr handle "content-length: "
                hPutStr handle $ S8.pack $ show $ S.length bs
                hPutStr handle "\r\n\r\n"
                threadDelay 100000
                hPutStr handle bs1
                threadDelay 100000
                hPutStr handle bs2
                hClose handle
                threadDelay 5000000
                front <- I.readIORef ifront
                S.concat (front []) `shouldBe` bs
    describe "raw body" $ do
        it "works" $ do
            let app _req = do
                    let backup = responseLBS status200 [] "Not raw"
                    return $ flip responseRaw backup $ \src sink ->
                        src
                            $$ Data.Conduit.List.map doubleBS
                            =$ sink
                doubleBS = S.concatMap $ \w -> S.pack [w, w]
            withApp defaultSettings app $ \port -> do
                handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
                hPutStr handle "POST / HTTP/1.1\r\n\r\n12345"
                hFlush handle
                timeout 100000 (S.hGet handle 10) >>= (`shouldBe` Just "1122334455")
                hPutStr handle "67890"
                hFlush handle
                timeout 100000 (S.hGet handle 10) >>= (`shouldBe` Just "6677889900")

    it "only one date and server header" $ do
        let app _ = return $ responseLBS status200
                [ ("server", "server")
                , ("date", "date")
                ] ""
        withApp defaultSettings app $ \port -> do
            Right res <- HTTP.simpleHTTP (HTTP.getRequest $ "http://127.0.0.1:" ++ show port)
            map HTTP.hdrValue (HTTP.retrieveHeaders HTTP.HdrServer res)
                `shouldBe` ["server"]
            map HTTP.hdrValue (HTTP.retrieveHeaders HTTP.HdrDate res)
                `shouldBe` ["date"]
