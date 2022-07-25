{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module RunSpec (main, spec, withApp, MySocket, msWrite, msRead, withMySocket) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.STM
import qualified UnliftIO.Exception as E
import UnliftIO.Exception (bracket, try, IOException, onException)
import Control.Monad (forM_, replicateM_, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
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
import Network.Wai.Internal (getRequestBodyChunk)
import Network.Wai.Handler.Warp
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)
import Test.Hspec

import HTTP

main :: IO ()
main = hspec spec

type Counter = I.IORef (Either String Int)
type CounterApplication = Counter -> Application

data MySocket = MySocket
  { msSocket :: !Socket
  , msBuffer :: !(I.IORef ByteString)
  }

msWrite :: MySocket -> ByteString -> IO ()
msWrite = sendAll . msSocket

msRead :: MySocket -> Int -> IO ByteString
msRead (MySocket s ref) expected = do
  bs <- I.readIORef ref
  inner (bs:) (S.length bs)
  where
    inner front total =
      case compare total expected of
        EQ -> do
          I.writeIORef ref mempty
          pure $ S.concat $ front []
        GT -> do
          let bs = S.concat $ front []
              (x, y) = S.splitAt expected bs
          I.writeIORef ref y
          pure x
        LT -> do
          bs <- safeRecv s 4096
          if S.null bs
            then do
              I.writeIORef ref mempty
              pure $ S.concat $ front []
            else inner (front . (bs:)) (total + S.length bs)

msClose :: MySocket -> IO ()
msClose = Network.Socket.close . msSocket

connectTo :: Int -> IO MySocket
connectTo port = do
    s <- fst <$> getSocketTCP "127.0.0.1" port
    ref <- I.newIORef mempty
    return MySocket {
        msSocket = s
      , msBuffer = ref
      }

withMySocket :: (MySocket -> IO a) -> Int -> IO a
withMySocket body port = bracket (connectTo port) msClose body

incr :: MonadIO m => Counter -> m ()
incr icount = liftIO $ I.atomicModifyIORef icount $ \ecount ->
    (case ecount of
        Left s -> Left s
        Right i -> Right $ i + 1, ())

err :: (MonadIO m, Show a) => Counter -> a -> m ()
err icount msg = liftIO $ I.writeIORef icount $ Left $ show msg

readBody :: CounterApplication
readBody icount req f = do
    body <- consumeBody $ getRequestBodyChunk req
    case () of
        ()
            | pathInfo req == ["hello"] && L.fromChunks body /= "Hello"
                -> err icount ("Invalid hello" :: String, body)
            | requestMethod req == "GET" && L.fromChunks body /= ""
                -> err icount ("Invalid GET" :: String, body)
            | requestMethod req `notElem` ["GET", "POST"]
                -> err icount ("Invalid request method (readBody)" :: String, requestMethod req)
            | otherwise -> incr icount
    f $ responseLBS status200 [] "Read the body"

ignoreBody :: CounterApplication
ignoreBody icount req f = do
    if requestMethod req `elem` ["GET", "POST"]
        then incr icount
        else err icount ("Invalid request method" :: String, requestMethod req)
    f $ responseLBS status200 [] "Ignored the body"

doubleConnect :: CounterApplication
doubleConnect icount req f = do
    _ <- consumeBody $ getRequestBodyChunk req
    _ <- consumeBody $ getRequestBodyChunk req
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
        (const $ do
            takeMVar baton
            -- use timeout to make sure we don't take too long
            mres <- timeout (60 * 1000 * 1000) (f port)
            case mres of
              Nothing -> error "Timeout triggered, too slow!"
              Just a -> pure a)

runTest :: Int -- ^ expected number of requests
        -> CounterApplication
        -> [ByteString] -- ^ chunks to send
        -> IO ()
runTest expected app chunks = do
    ref <- I.newIORef (Right 0)
    withApp defaultSettings (app ref) $ withMySocket $ \ms -> do
        forM_ chunks $ \chunk -> msWrite ms chunk
        _ <- timeout 100000 $ replicateM_ expected $ msRead ms 4096
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
    withApp (setOnException onExc defaultSettings) dummyApp $ withMySocket $ \ms -> do
        msWrite ms input
        msClose ms -- explicitly
        threadDelay 5000
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
        it "chunked body, read" $ runTest 2 readBody $
            singleChunkedPostHello ++ [singleGet]
        it "chunked body, ignore" $ runTest 2 ignoreBody $
            singleChunkedPostHello ++ [singleGet]
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
        it "IncompleteHeaders" $
            runTerminateTest IncompleteHeaders "GET / HTTP/1.1\r\ncontent-length: 10\r\n"

    describe "special input" $ do
        it "multiline headers" $ do
            iheaders <- I.newIORef []
            let app req f = do
                    liftIO $ I.writeIORef iheaders $ requestHeaders req
                    f $ responseLBS status200 [] ""
            withApp defaultSettings app $ withMySocket $ \ms -> do
                let input = S.concat
                        [ "GET / HTTP/1.1\r\nfoo:    bar\r\n baz\r\n\tbin\r\n\r\n"
                        ]
                msWrite ms input
                threadDelay 5000
                headers <- I.readIORef iheaders
                headers `shouldBe`
                    [ ("foo", "bar baz\tbin")
                    ]
        it "no space between colon and value" $ do
            iheaders <- I.newIORef []
            let app req f = do
                    liftIO $ I.writeIORef iheaders $ requestHeaders req
                    f $ responseLBS status200 [] ""
            withApp defaultSettings app $ withMySocket $ \ms -> do
                let input = S.concat
                        [ "GET / HTTP/1.1\r\nfoo:bar\r\n\r\n"
                        ]
                msWrite ms input
                threadDelay 5000
                headers <- I.readIORef iheaders
                headers `shouldBe`
                    [ ("foo", "bar")
                    ]

    describe "chunked bodies" $ do
        it "works" $ do
            countVar <- newTVarIO (0 :: Int)
            ifront <- I.newIORef id
            let app req f = do
                    bss <- consumeBody $ getRequestBodyChunk req
                    liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                    atomically $ modifyTVar countVar (+ 1)
                    f $ responseLBS status200 [] ""
            withApp defaultSettings app $ withMySocket $ \ms -> do
                let input = S.concat
                        [ "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"
                        , "c\r\nHello World\n\r\n3\r\nBye\r\n0\r\n\r\n"
                        , "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"
                        , "b\r\nHello World\r\n0\r\n\r\n"
                        ]
                msWrite ms input
                atomically $ do
                  count <- readTVar countVar
                  check $ count == 2
                front <- I.readIORef ifront
                front [] `shouldBe`
                    [ "Hello World\nBye"
                    , "Hello World"
                    ]
        it "lots of chunks" $ do
            ifront <- I.newIORef id
            countVar <- newTVarIO (0 :: Int)
            let app req f = do
                    bss <- consumeBody $ getRequestBodyChunk req
                    I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                    atomically $ modifyTVar countVar (+ 1)
                    f $ responseLBS status200 [] ""
            withApp defaultSettings app $ withMySocket $ \ms -> do
                let input = concat $ replicate 2 $
                        ["POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"] ++
                        replicate 50 "5\r\n12345\r\n" ++
                        ["0\r\n\r\n"]
                mapM_ (msWrite ms) input
                atomically $ do
                  count <- readTVar countVar
                  check $ count == 2
                front <- I.readIORef ifront
                front [] `shouldBe` replicate 2 (S.concat $ replicate 50 "12345")
-- For some reason, the following test on Windows causes the socket
-- to be killed prematurely. Worth investigating in the future if possible.
        it "in chunks" $ do
            ifront <- I.newIORef id
            countVar <- newTVarIO (0 :: Int)
            let app req f = do
                    bss <- consumeBody $ getRequestBodyChunk req
                    liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                    atomically $ modifyTVar countVar (+ 1)
                    f $ responseLBS status200 [] ""
            withApp defaultSettings app $ withMySocket $ \ms -> do
                let input = S.concat
                        [ "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"
                        , "c\r\nHello World\n\r\n3\r\nBye\r\n0\r\n"
                        , "POST / HTTP/1.1\r\nTransfer-Encoding: Chunked\r\n\r\n"
                        , "b\r\nHello World\r\n0\r\n\r\n"
                        ]
                mapM_ (msWrite ms . S.singleton) $ S.unpack input
                atomically $ do
                  count <- readTVar countVar
                  check $ count == 2
                front <- I.readIORef ifront
                front [] `shouldBe`
                    [ "Hello World\nBye"
                    , "Hello World"
                    ]
        it "timeout in request body" $ do
            ifront <- I.newIORef id
            let app req f = do
                    bss <- consumeBody (getRequestBodyChunk req) `onException`
                        liftIO (I.atomicModifyIORef ifront (\front -> (front . ("consume interrupted":), ())))
                    liftIO $ threadDelay 4000000 `E.catch` \e -> do
                        I.atomicModifyIORef ifront (\front ->
                            ( front . ((S8.pack $ "threadDelay interrupted: " ++ show e):)
                            , ()))
                        E.throwIO (e :: E.SomeException)
                    liftIO $ I.atomicModifyIORef ifront $ \front -> (front . (S.concat bss:), ())
                    f $ responseLBS status200 [] ""
            withApp (setTimeout 1 defaultSettings) app $ withMySocket $ \ms -> do
                let bs1 = S.replicate 2048 88
                    bs2 = "This is short"
                    bs = S.append bs1 bs2
                msWrite ms "POST / HTTP/1.1\r\n"
                msWrite ms "content-length: "
                msWrite ms $ S8.pack $ show $ S.length bs
                msWrite ms "\r\n\r\n"
                threadDelay 100000
                msWrite ms bs1
                threadDelay 100000
                msWrite ms bs2
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
            withApp defaultSettings app $ withMySocket $ \ms -> do
                msWrite ms "POST / HTTP/1.1\r\n\r\n12345"
                timeout 100000 (msRead ms 10) >>= (`shouldBe` Just "1122334455")
                msWrite ms "67890"
                timeout 100000 (msRead ms 10) >>= (`shouldBe` Just "6677889900")
    it "only one date and server header" $ do
        let app _ f = f $ responseLBS status200
                [ ("server", "server")
                , ("date", "date")
                ] ""
            getValues key = map snd
                          . filter (\(key', _) -> key == key')
                          . responseHeaders
        withApp defaultSettings app $ \port -> do
            res <- sendGET $ "http://127.0.0.1:" ++ show port
            getValues hServer res `shouldBe` ["server"]
            getValues hDate res `shouldBe` ["date"]

    it "streaming echo #249" $ do
        countVar <- newTVarIO (0 :: Int)
        let app req f = f $ responseStream status200 [] $ \write _ -> do
             let loop = do
                    bs <- getRequestBodyChunk req
                    unless (S.null bs) $ do
                        write $ byteString bs
                        atomically $ modifyTVar countVar (+ 1)
                        loop
             loop
        withApp defaultSettings app $ withMySocket $ \ms -> do
            msWrite ms "POST / HTTP/1.1\r\ntransfer-encoding: chunked\r\n\r\n"
            threadDelay 10000
            msWrite ms "5\r\nhello\r\n0\r\n\r\n"
            atomically $ do
              count <- readTVar countVar
              check $ count >= 1
            bs <- safeRecv (msSocket ms) 4096 -- must not use msRead
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
