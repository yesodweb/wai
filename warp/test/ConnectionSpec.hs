{-# LANGUAGE OverloadedStrings #-}

module ConnectionSpec (spec) where

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp
import RunSpec (withApp, withMySocket, msWrite, msRead)
import Test.Hspec
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as S8

spec :: Spec
spec = describe "Connection header" $ do
    it "sends Connection: close when response implies close (HTTP/1.0 Keep-Alive, no Content-Length)" $ do
        let app _ f = f $ responseLBS status200 [] "foo"
        withApp defaultSettings app $ withMySocket $ \ms -> do
            -- HTTP/1.0 defaults to close. We ask for Keep-Alive.
            -- But we provide no Content-Length in response.
            -- So Warp should decide to close (because it can't keep alive without length or chunking),
            -- and MUST send "Connection: close" to inform the client.
            msWrite ms "GET / HTTP/1.0\r\nConnection: Keep-Alive\r\n\r\n"

            -- We expect the connection to be closed by the server, so reading a large amount
            -- should return whatever was sent and then finish.
            response <- msRead ms 4096

            let headers = parseHeaders response
            lookup "Connection" headers `shouldBe` Just "close"

parseHeaders :: ByteString -> [(ByteString, ByteString)]
parseHeaders bs = 
    let lines = S8.lines bs
        -- Drop status line
        headerLines = takeWhile (not . S8.null . S8.filter (/= '\r')) $ drop 1 lines
        parseLine line = 
            let (k, v) = S8.break (== ':') line
                v' = S8.takeWhile (/= '\r') v
            in (k, S8.dropWhile (== ' ') $ S8.drop 1 v')
    in map parseLine headerLines
