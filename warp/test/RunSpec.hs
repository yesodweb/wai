{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunSpec (main, spec, withApp, connectTo) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import qualified Control.Exception as E
import Control.Exception.Lifted (bracket, try, IOException, onException)
import Control.Monad (forM_, replicateM_, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString, hPutStr, hGetSome)
import qualified Data.ByteString as S
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified Data.IORef as I
import Data.Streaming.Network (bindPortTCP, getSocketTCP, safeRecv)
import Network.HTTP.Types
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Network.Wai hiding (responseHeaders)
import Network.Wai.Handler.Warp
import System.IO (hFlush, hClose, Handle, IOMode(..))
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.Hspec

import HTTP

main :: IO ()
main = hspec spec

type Counter = I.IORef (Either String Int)
type CounterApplication = Counter -> Application

connectTo :: HostName -> Int -> IO Handle
connectTo host port = do
    let hints = defaultHints { addrSocketType = Stream }
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr
    socketToHandle sock ReadWriteMode

incr :: MonadIO m => Counter -> m ()
incr icount = liftIO $ I.atomicModifyIORef icount $ \ecount ->
    ((case ecount of
        Left s -> Left s
        Right i -> Right $ i + 1), ())

err :: (MonadIO m, Show a) => Counter -> a -> m ()
err icount msg = liftIO $ I.writeIORef icount $ Left $ show msg

readBody :: CounterApplication
readBody icount req f = do
    body <- consumeBody $ requestBody req
    case () of
        ()
            | pathInfo req == ["hello"] && L.fromChunks body /= "Hello"
                -> err icount ("Invalid hello" :: String, body)
            | requestMethod req == "GET" && L.fromChunks body /= ""
                -> err icount ("Invalid GET" :: String, body)
            | not $ requestMethod req `elem` ["GET", "POST"]
                -> err icount ("Invalid request method (readBody)" :: String, requestMethod req)
            | otherwise -> incr icount
    f $ responseLBS status200 [] "Read the body"

ignoreBody :: CounterApplication
ignoreBody icount req f = do
    if (requestMethod req `elem` ["GET", "POST"])
        then incr icount
        else err icount ("Invalid request method" :: String, requestMethod req)
    f $ responseLBS status200 [] "Ignored the body"

doubleConnect :: CounterApplication
doubleConnect icount req f = do
    _ <- consumeBody $ requestBody req
    _ <- consumeBody $ requestBody req
    incr icount
    f $ responseLBS status200 [] "double connect"

nextPort :: I.IORef Int
nextPort = unsafePerformIO $ I.newIORef 5000
{-# NOINLINE nextPort #-}

getPort :: IO Int
getPort = do
    port <- I.atomicModifyIORef nextPort $ \p -> (p + 1, p)
    esocket <- try $ bindPortTCP port "127.0.0.1"
    case esocket of
        Left (_ :: IOException) -> RunSpec.getPort
        Right sock -> do
            close sock
            return port

withApp :: Settings -> Application -> (Int -> IO a) -> IO a
withApp settings app f = do
    port <- RunSpec.getPort
    baton <- newEmptyMVar
    let settings' = setPort port
                  $ setHost "127.0.0.1"
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
        handle <- connectTo "127.0.0.1" port
        forM_ chunks $ \chunk -> hPutStr handle chunk >> hFlush handle
        _ <- timeout 100000 $ replicateM_ expected $ hGetSome handle 4096
        res <- I.readIORef ref
        case res of
            Left s -> error s
            Right i -> i `shouldBe` expected

dummyApp :: Application
dummyApp _ f = f $ responseLBS status200 [] "foo"

runTerminateTest :: InvalidRequest
                 -> ByteString
                 -> IO ()
runTerminateTest expected input = do
    ref <- I.newIORef Nothing
    let onExc _ = I.writeIORef ref . Just
    withApp (setOnException onExc defaultSettings) dummyApp $ \port -> do
        handle <- connectTo "127.0.0.1" port
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

singleChunkedPostHello :: [ByteString]
singleChunkedPostHello =
    [ "POST /hello HTTP/1.1\r\nHost: localhost\r\nTransfer-Encoding: chunked\r\n\r\n"
    , "5\r\nHello\r\n0\r\n"
    ]

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
        it "chunked body, read" $ runTest 2 readBody $ concat
            [ singleChunkedPostHello
            , [singleGet]
            ]
        it "chunked body, ignore" $ runTest 2 ignoreBody $ concat
            [ singleChunkedPostHello
            , [singleGet]
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
        it "chunked body, read" $ runTest 2 readBody $ return $ S.concat
            [ S.concat singleChunkedPostHello
            , singleGet
            ]
        it "chunked body, ignore" $ runTest 2 ignoreBody $ return $ S.concat
            [ S.concat singleChunkedPostHello
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
            let app req f = do
                    liftIO $ I.writeIORef iheaders $ requestHeaders req
                    f $ responseLBS status200 [] ""
            withApp defaultSettings app $ \port -> do
                handle <- connectTo "127.0.0.1" port
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
            let app req f = do
                    liftIO $ I.writeIORef iheaders $ requestHeaders req
                    f $ responseLBS status200 [] ""
            withApp defaultSettings app $ \port -> do
                handle <- connectTo "127.0.0.1" port
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
            let app req f = do
                    bss <- consumeBody $ requestBody req
                    liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                    f $ responseLBS status200 [] ""
            withApp defaultSettings app $ \port -> do
                handle <- connectTo "127.0.0.1" port
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
            let app req f = do
                    bss <- consumeBody $ requestBody req
                    I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                    f $ responseLBS status200 [] ""
            withApp defaultSettings app $ \port -> do
                handle <- connectTo "127.0.0.1" port
                let input = concat $ replicate 2 $
                        ["POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"] ++
                        (replicate 50 "5\r\n12345\r\n") ++
                        ["0\r\n\r\n"]
                mapM_ (\bs -> hPutStr handle bs >> hFlush handle) input
                hClose handle
                threadDelay 100000 -- FIXME why does this delay need to be so high?
                front <- I.readIORef ifront
                front [] `shouldBe` replicate 2 (S.concat $ replicate 50 "12345")
        it "in chunks" $ do
            ifront <- I.newIORef id
            let app req f = do
                    bss <- consumeBody $ requestBody req
                    liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                    f $ responseLBS status200 [] ""
            withApp defaultSettings app $ \port -> do
                handle <- connectTo "127.0.0.1" port
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
            let app req f = do
                    bss <- (consumeBody $ requestBody req) `onException`
                        liftIO (I.atomicModifyIORef ifront (\front -> (front . ("consume interrupted":), ())))
                    liftIO $ threadDelay 4000000 `E.catch` \e -> do
                        I.atomicModifyIORef ifront (\front ->
                            ( front . ((S8.pack $ "threadDelay interrupted: " ++ show e):)
                            , ()))
                        E.throwIO (e :: E.SomeException)
                    liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                    f $ responseLBS status200 [] ""
            withApp (setTimeout 1 defaultSettings) app $ \port -> do
                let bs1 = S.replicate 2048 88
                    bs2 = "This is short"
                    bs = S.append bs1 bs2
                handle <- connectTo "127.0.0.1" port
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
            let app _req f = do
                    let backup = responseLBS status200 [] "Not raw"
                    f $ flip responseRaw backup $ \src sink -> do
                        let loop = do
                                bs <- src
                                unless (S.null bs) $ do
                                    sink $ doubleBS bs
                                    loop
                        loop
                doubleBS = S.concatMap $ \w -> S.pack [w, w]
            withApp defaultSettings app $ \port -> do
                handle <- connectTo "127.0.0.1" port
                hPutStr handle "POST / HTTP/1.1\r\n\r\n12345"
                hFlush handle
                timeout 100000 (S.hGet handle 10) >>= (`shouldBe` Just "1122334455")
                hPutStr handle "67890"
                hFlush handle
                timeout 100000 (S.hGet handle 10) >>= (`shouldBe` Just "6677889900")

    it "only one date and server header" $ do
        let app _ f = f $ responseLBS status200
                [ ("server", "server")
                , ("date", "date")
                ] ""
        withApp defaultSettings app $ \port -> do
            res <- sendGET $ "http://127.0.0.1:" ++ show port
            getHeaderValue hServer (responseHeaders res) `shouldBe` Just "server"
            getHeaderValue hDate (responseHeaders res) `shouldBe` Just "date"

    it "streaming echo #249" $ do
        let app req f = f $ responseStream status200 [] $ \write _ -> do
            let loop = do
                    bs <- requestBody req
                    unless (S.null bs) $ do
                        write $ byteString bs
                        loop
            loop
        withApp defaultSettings app $ \port -> do
            (sock, _addr) <- getSocketTCP "127.0.0.1" port
            sendAll sock "POST / HTTP/1.1\r\ntransfer-encoding: chunked\r\n\r\n"
            threadDelay 10000
            sendAll sock "5\r\nhello\r\n0\r\n\r\n"
            bs <- safeRecv sock 4096
            S.takeWhile (/= 13) bs `shouldBe` "HTTP/1.1 200 OK"

    it "streaming response with length" $ do
        let app _ f = f $ responseStream status200 [("content-length", "20")] $ \write _ -> do
                replicateM_ 4 $ write $ byteString "Hello"
        withApp defaultSettings app $ \port -> do
            res <- sendGET $ "http://127.0.0.1:" ++ show port
            responseBody res `shouldBe` "HelloHelloHelloHello"

    describe "head requests" $ do
        let fp = "test/head-response"
        let app req f =
                f $ case pathInfo req of
                    ["builder"] -> responseBuilder status200 [] $ error "should never be evaluated"
                    ["streaming"] -> responseStream status200 [] $ \write _ ->
                        write $ error "should never be evaluated"
                    ["file"] -> responseFile status200 [] fp Nothing
                    _ -> error "invalid path"
        it "builder" $ withApp defaultSettings app $ \port -> do
            res <- sendHEAD $ concat ["http://127.0.0.1:", show port, "/builder"]
            responseBody res `shouldBe` ""
        it "streaming" $ withApp defaultSettings app $ \port -> do
            res <- sendHEAD $ concat ["http://127.0.0.1:", show port, "/streaming"]
            responseBody res `shouldBe` ""
        it "file, no range" $ withApp defaultSettings app $ \port -> do
            bs <- S.readFile fp
            res <- sendHEAD $ concat ["http://127.0.0.1:", show port, "/file"]
            getHeaderValue hContentLength (responseHeaders res) `shouldBe` Just (S8.pack $ show $ S.length bs)
        it "file, with range" $ withApp defaultSettings app $ \port -> do
            res <- sendHEADwH
                (concat ["http://127.0.0.1:", show port, "/file"])
                [(hRange, "bytes=0-1")]
            getHeaderValue hContentLength (responseHeaders res) `shouldBe` Just "2"

consumeBody :: IO ByteString -> IO [ByteString]
consumeBody body =
    loop id
  where
    loop front = do
        bs <- body
        if S.null bs
            then return $ front []
            else loop $ front . (bs:)
