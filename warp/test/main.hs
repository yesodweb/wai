{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.IORef as I
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Types
import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Monad (forM_)

import System.IO (hFlush)
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

singleGet :: ByteString
singleGet = "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"

singlePostHello :: ByteString
singlePostHello = "POST /hello HTTP/1.1\r\nHost: localhost\r\nContent-length: 5\r\n\r\nHello"

main :: IO ()
main = hspecX $ do
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
