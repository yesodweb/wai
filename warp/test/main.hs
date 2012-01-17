{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.IORef as I
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Types
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forM_)

import System.IO (hFlush)
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

runTest :: Int -- ^ expected number of requests
        -> Int -- ^ port to run on
        -> CounterApplication
        -> [ByteString] -- ^ chunks to send
        -> IO ()
runTest expected port app chunks = do
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

singleGet :: ByteString
singleGet = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"

singlePostHello :: ByteString
singlePostHello = "POST /hello HTTP/1.1\r\nHost: localhost\r\nContent-length: 5\r\n\r\nHello"

main :: IO ()
main = hspecX $ do
    describe "non-pipelining" $ do
        it "no body, read" $ runTest 5 5007 readBody $ replicate 5 singleGet
        it "no body, ignore" $ runTest 5 5003 ignoreBody $ replicate 5 singleGet
        it "has body, read" $ runTest 2 5004 readBody
            [ singlePostHello
            , singleGet
            ]
        it "has body, ignore" $ runTest 2 5005 ignoreBody
            [ singlePostHello
            , singleGet
            ]
    describe "pipelining" $ do
        it "no body, read" $ runTest 5 5006 readBody [S.concat $ replicate 5 singleGet]
        it "no body, ignore" $ runTest 5 5002 ignoreBody [S.concat $ replicate 5 singleGet]
        it "has body, read" $ runTest 2 5001 readBody $ return $ S.concat
            [ singlePostHello
            , singleGet
            ]
        it "has body, ignore" $ runTest 2 5000 ignoreBody $ return $ S.concat
            [ singlePostHello
            , singleGet
            ]
