{-# LANGUAGE OverloadedStrings #-}

module ConnectionSpec (spec) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import RunSpec (msRead, msWrite, withApp, withMySocket)
import Test.Hspec

spec :: Spec
spec = describe "Connection header" $ do
    describe "HTTP/1.0 Connection: close behavior" $ do
        -- HTTP/1.0 defaults to close. We ask for Keep-Alive.
        -- But we provide no Content-Length in response.
        -- So Warp should decide to close (because it can't keep alive without length or chunking),
        -- and MUST send "Connection: close" to inform the client.
        -- (In HTTP/1.0 this requires closing the connection to delimit the
        -- response; or rather, lack of persistence info)
        testClose
            "when response implies close (HTTP/1.0 Keep-Alive, but no Content-Length)"
            (responseLBS status200 [] "foo")
            "GET / HTTP/1.0\r\nConnection: Keep-Alive\r\n\r\n"
        testClose
            "sends \"Connection: close\" on regular HTTP/1.0 GET request"
            (responseBuilder status200 [] "foo")
            "GET / HTTP/1.0\r\nHost: localhost\r\n\r\n"

    describe "HTTP/1.1 Connection: close behavior" $ do
        -- Response has no Content-Length and is not chunked (HEAD implies no body).
        it "does NOT send \"Connection: close\" for HTTP/1.1 HEAD request" $ do
            let app _ f = f $ responseBuilder status200 [] "foo"
            withApp defaultSettings app $ withMySocket $ \ms -> do
                msWrite ms "HEAD / HTTP/1.1\r\nHost: localhost\r\n\r\n"
                -- Should include the Connection header if present and also
                -- should be less then all headers when it's absent, so we
                -- don't wait for nothing.
                response <- msRead ms 73
                let headers = parseHeaders response
                lookup "Connection" headers `shouldBe` Nothing
        testClose
            "when GET request has \"Connection: close\" (200 OK)"
            (responseLBS status200 [] "foo")
            "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        testClose
            "when HEAD request has \"Connection: close\" (200 OK)"
            (responseLBS status200 [] "foo")
            "HEAD / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        testClose
            "when request has \"Connection: close\" (204 No Content)"
            (responseLBS status204 [] "")
            "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
        testClose
            "when request has \"Connection: close\" (500 Internal Server Error)"
            (responseLBS status500 [] "error")
            "GET / HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n"
  where
    testClose name res input = do
        let prefix = "sends \"Connection: close\" "
        it (prefix <> name) $ do
            let app _ f = f res
            withApp defaultSettings app $ withMySocket $ \ms -> do
                msWrite ms input
                -- We expect the connection to be closed by the server, so reading a large amount
                -- should return whatever was sent and then finish.
                response <- msRead ms 4096
                let headers = parseHeaders response
                lookup "Connection" headers `shouldBe` Just "close"

parseHeaders :: ByteString -> [(ByteString, ByteString)]
parseHeaders bs =
    let allLines = S8.lines bs
        -- Drop status line
        headerLines = takeWhile (not . S8.null . S8.filter (/= '\r')) $ drop 1 allLines
        parseLine line =
            let (k, v) = S8.break (== ':') line
                v' = S8.takeWhile (/= '\r') v
             in (k, S8.dropWhile (== ' ') $ S8.drop 1 v')
     in map parseLine headerLines
