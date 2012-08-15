{-# LANGUAGE OverloadedStrings #-}

module RunSpec where

import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.IORef as I
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Types
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forM_)

import System.IO (hFlush, hClose)
import System.IO.Unsafe (unsafePerformIO)
import Data.ByteString (ByteString, hPutStr, hGetSome)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Network (connectTo, PortID (PortNumber))

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit

import Data.Conduit (($$))
import qualified Data.Conduit.List

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
getPort = I.atomicModifyIORef nextPort $ \p -> (p + 1, p)

runTest :: Int -- ^ expected number of requests
        -> CounterApplication
        -> [ByteString] -- ^ chunks to send
        -> IO ()
runTest expected app chunks = do
    port <- getPort
    ref <- I.newIORef (Right 0)
    tid <- forkIO $ run port $ app ref
    threadDelay 1000
    handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
    forM_ chunks $ \chunk -> hPutStr handle chunk >> hFlush handle
    _ <- hGetSome handle 4096
    threadDelay 1000
    killThread tid
    res <- I.readIORef ref
    case res of
        Left s -> error s
        Right i -> i @?= expected

dummyApp :: Application
dummyApp _ = return $ responseLBS status200 [] "foo"

runTerminateTest :: InvalidRequest
                 -> ByteString
                 -> IO ()
runTerminateTest expected input = do
    port <- getPort
    ref <- I.newIORef Nothing
    tid <- forkIO $ runSettings defaultSettings
        { settingsOnException = \e -> I.writeIORef ref $ Just e
        , settingsPort = port
        } dummyApp
    threadDelay 1000
    handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
    hPutStr handle input
    hFlush handle
    hClose handle
    threadDelay 1000
    killThread tid
    res <- I.readIORef ref
    show res @?= show (Just expected)

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
        it "ConnectionClosedByPeer" $ runTerminateTest ConnectionClosedByPeer "GET / HTTP/1.1\r\ncontent-length: 10\r\n\r\nhello"
        it "IncompleteHeaders" $ runTerminateTest IncompleteHeaders "GET / HTTP/1.1\r\ncontent-length: 10\r\n"

    describe "special input" $ do
        it "multiline headers" $ do
            iheaders <- I.newIORef []
            port <- getPort
            tid <- forkIO $ run port $ \req -> do
                liftIO $ I.writeIORef iheaders $ requestHeaders req
                return $ responseLBS status200 [] ""
            threadDelay 1000
            handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
            let input = S.concat
                    [ "GET / HTTP/1.1\r\nfoo:    bar\r\n baz\r\n\tbin\r\n\r\n"
                    ]
            hPutStr handle input
            hFlush handle
            hClose handle
            threadDelay 1000
            killThread tid
            headers <- I.readIORef iheaders
            headers @?=
                [ ("foo", "bar baz\tbin")
                ]
        it "no space between colon and value" $ do
            iheaders <- I.newIORef []
            port <- getPort
            tid <- forkIO $ run port $ \req -> do
                liftIO $ I.writeIORef iheaders $ requestHeaders req
                return $ responseLBS status200 [] ""
            threadDelay 1000
            handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
            let input = S.concat
                    [ "GET / HTTP/1.1\r\nfoo:bar\r\n\r\n"
                    ]
            hPutStr handle input
            hFlush handle
            hClose handle
            threadDelay 1000
            killThread tid
            headers <- I.readIORef iheaders
            headers @?=
                [ ("foo", "bar")
                ]
        it "extra spaces in first line" $ do
            iheaders <- I.newIORef []
            port <- getPort
            tid <- forkIO $ run port $ \req -> do
                liftIO $ I.writeIORef iheaders $ requestHeaders req
                return $ responseLBS status200 [] ""
            threadDelay 1000
            handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
            let input = S.concat
                    [ "GET    /    HTTP/1.1\r\nfoo: bar\r\n\r\n"
                    ]
            hPutStr handle input
            hFlush handle
            hClose handle
            threadDelay 1000
            killThread tid
            headers <- I.readIORef iheaders
            headers @?=
                [ ("foo", "bar")
                ]
        it "spaces in http version" $ do
            iversion <- I.newIORef $ error "Version not parsed"
            port <- getPort
            tid <- forkIO $ run port $ \req -> do
                liftIO $ I.writeIORef iversion $ httpVersion req
                return $ responseLBS status200 [] ""
            threadDelay 1000
            handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
            let input = S.concat
                    [ "GET    /    HTTP\t/  1 .   1  \r\nfoo: bar\r\n\r\n"
                    ]
            hPutStr handle input
            hFlush handle
            hClose handle
            threadDelay 1000
            killThread tid
            version <- I.readIORef iversion
            version @?= http11

    describe "chunked bodies" $ do
        it "works" $ do
            ifront <- I.newIORef id
            port <- getPort
            tid <- forkIO $ run port $ \req -> do
                bss <- requestBody req $$ Data.Conduit.List.consume
                liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                return $ responseLBS status200 [] ""
            threadDelay 1000
            handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
            let input = S.concat
                    [ "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"
                    , "c\r\nHello World\n\r\n3\r\nBye\r\n0\r\n"
                    , "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"
                    , "b\r\nHello World\r\n0\r\n"
                    ]
            hPutStr handle input
            hFlush handle
            hClose handle
            threadDelay 1000
            killThread tid
            front <- I.readIORef ifront
            front [] @?=
                [ "Hello World\nBye"
                , "Hello World"
                ]
        it "lots of chunks" $ do
            ifront <- I.newIORef id
            port <- getPort
            tid <- forkIO $ run port $ \req -> do
                bss <- requestBody req $$ Data.Conduit.List.consume
                liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                return $ responseLBS status200 [] ""
            threadDelay 1000
            handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
            let input = concat $ replicate 2 $
                    ["POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"] ++
                    (replicate 50 "5\r\n12345\r\n") ++
                    ["0\r\n"]
            mapM_ (\bs -> hPutStr handle bs >> hFlush handle) input
            hClose handle
            threadDelay 1000
            killThread tid
            front <- I.readIORef ifront
            front [] @?= replicate 2 (S.concat $ replicate 50 "12345")
        it "in chunks" $ do
            ifront <- I.newIORef id
            port <- getPort
            tid <- forkIO $ run port $ \req -> do
                bss <- requestBody req $$ Data.Conduit.List.consume
                liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                return $ responseLBS status200 [] ""
            threadDelay 1000
            handle <- connectTo "127.0.0.1" $ PortNumber $ fromIntegral port
            let input = S.concat
                    [ "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"
                    , "c\r\nHello World\n\r\n3\r\nBye\r\n0\r\n"
                    , "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"
                    , "b\r\nHello World\r\n0\r\n"
                    ]
            mapM_ (\bs -> hPutStr handle bs >> hFlush handle) $ map S.singleton $ S.unpack input
            hClose handle
            threadDelay 1000
            killThread tid
            front <- I.readIORef ifront
            front [] @?=
                [ "Hello World\nBye"
                , "Hello World"
                ]
