{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import qualified Data.IORef as I
import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Types
import Control.Concurrent (forkIO, killThread)

import System.IO (hFlush, hPutStrLn, stderr)
import Data.ByteString (hPutStr, hGetSome)
import qualified Data.ByteString.Lazy as L
import Network (connectTo, PortID (PortNumber))

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit

import Data.Conduit (($$))
import qualified Data.Conduit.List

print' :: (Show a, MonadIO m) => a -> m ()
print' = liftIO . hPutStrLn stderr . show

readBody :: I.IORef Int -> Application
readBody icount req = do
    body <- requestBody req $$ Data.Conduit.List.consume
    if (pathInfo req == ["hello"] && L.fromChunks body /= "Hello")
        then print' ("Invalid hello", body)
        else liftIO $ I.atomicModifyIORef icount $ \count -> (count + 1, ())
    return $ responseLBS status200 [] "Read the body"

ignoreBody :: I.IORef Int -> Application
ignoreBody icount req = do
    if (requestMethod req `elem` ["GET", "POST"])
        then liftIO $ I.atomicModifyIORef icount $ \count -> (count + 1, ())
        else print' ("Invalid request method", requestMethod req)
    return $ responseLBS status200 [] "Ignored the body"

main :: IO ()
main = hspecX $ do
    describe "non-pipelining" $ do
        it "no body" $ do
            icount <- I.newIORef 0
            tid <- forkIO $ run 5003 $ readBody icount
            handle <- connectTo "127.0.0.1" $ PortNumber 5003
            hPutStr handle "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
            hFlush handle
            res <- hGetSome handle 4096
            hPutStr handle "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
            hFlush handle
            res <- hGetSome handle 4096
            killThread tid
            count <- I.readIORef icount
            count @?= 2
        it "app reads the body" $ do
            icount <- I.newIORef 0
            tid <- forkIO $ run 5004 $ readBody icount
            handle <- connectTo "127.0.0.1" $ PortNumber 5004
            hPutStr handle "POST /hello HTTP/1.1\r\nHost: localhost\r\nContent-length: 5\r\n\r\nHello"
            hFlush handle
            res <- hGetSome handle 4096
            hPutStr handle "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
            hFlush handle
            res <- hGetSome handle 4096
            killThread tid
            count <- I.readIORef icount
            count @?= 2
        it "app ignores the body" $ do
            icount <- I.newIORef 0
            tid <- forkIO $ run 5005 $ ignoreBody icount
            handle <- connectTo "127.0.0.1" $ PortNumber 5005
            hPutStr handle "POST /hello HTTP/1.1\r\nHost: localhost\r\nContent-length: 5\r\n\r\nHello"
            hFlush handle
            res <- hGetSome handle 4096
            hPutStr handle "GET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
            hFlush handle
            res <- hGetSome handle 4096
            killThread tid
            count <- I.readIORef icount
            count @?= 2
    describe "pipelining" $ do
        it "no body" $ do
            icount <- I.newIORef 0
            tid <- forkIO $ run 5002 $ readBody icount
            handle <- connectTo "127.0.0.1" $ PortNumber 5002
            hPutStr handle "GET / HTTP/1.1\r\nHost: localhost\r\n\r\nGET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
            hFlush handle
            res <- hGetSome handle 4096
            print res
            killThread tid
            count <- I.readIORef icount
            count @?= 2
        it "app reads the body" $ do
            icount <- I.newIORef 0
            tid <- forkIO $ run 5001 $ readBody icount
            handle <- connectTo "127.0.0.1" $ PortNumber 5001
            hPutStr handle "POST / HTTP/1.1\r\nHost: localhost\r\nContent-Length: 5\r\n\r\nHello\r\n\r\nGET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
            hFlush handle
            res <- hGetSome handle 4096
            print res
            killThread tid
            count <- I.readIORef icount
            count @?= 2
        it "app ignores the body" $ do
            icount <- I.newIORef 0
            tid <- forkIO $ run 5000 $ ignoreBody icount
            handle <- connectTo "127.0.0.1" $ PortNumber 5000
            hPutStr handle "POST / HTTP/1.1\r\nHost: localhost\r\nContent-Length: 5\r\n\r\nHello\r\nGET / HTTP/1.1\r\nHost: localhost\r\n\r\n"
            hFlush handle
            res <- hGetSome handle 4096
            print res
            killThread tid
            count <- I.readIORef icount
            count @?= 2
