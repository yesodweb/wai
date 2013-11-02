{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception (throwIO)
import Control.Monad
import qualified Data.ByteString as S
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Network.HTTP.Types as H
import Network.Wai.Handler.Warp.Types
import Prelude hiding (lines)

import Criterion.Main

-- $setup
-- >>> :set -XOverloadedStrings

main :: IO ()
main = do
    let requestLine1 = "GET http://www.example.com HTTP/1.1"
    let requestLine2 = "GET http://www.example.com/cgi-path/search.cgi?key=parser HTTP/1.0"
    defaultMain [
        bgroup "requestLine1" [
             bench "parseRequestLine1" $ parseRequestLine1 requestLine1
           , bench "parseRequestLine0" $ parseRequestLine0 requestLine1
           ]
      , bgroup "requestLine2" [
             bench "parseRequestLine1" $ parseRequestLine1 requestLine2
           , bench "parseRequestLine0" $ parseRequestLine0 requestLine2
           ]
      ]

----------------------------------------------------------------

parseRequestLine2 :: ByteString
                 -> IO (H.Method
                       ,ByteString -- Path
                       ,ByteString -- Query
                       ,H.HttpVersion)
parseRequestLine2 requestLine = undefined

----------------------------------------------------------------

-- |
--
-- >>> parseRequestLine1 "GET / HTTP/1.1"
-- ("GET","/","",HTTP/1.1)
-- >>> parseRequestLine1 "POST /cgi/search.cgi?key=foo HTTP/1.0"
-- ("POST","/cgi/search.cgi","?key=foo",HTTP/1.0)
-- >>> parseRequestLine1 "GET /NoHTTPVersion"
-- *** Exception: BadFirstLine "GET /NoHTTPVersion"
-- >>> parseRequestLine1 "GET /NotHTTP UNKNOWN/1.1"
-- *** Exception: NonHttp
parseRequestLine1 :: ByteString
                 -> IO (H.Method
                       ,ByteString -- Path
                       ,ByteString -- Query
                       ,H.HttpVersion)
parseRequestLine1 requestLine = do
    let (!method,!rest) = S.breakByte 32 requestLine -- ' '
        (!pathQuery,!httpVer') = S.breakByte 32 (S.drop 1 rest) -- ' '
        !httpVer = S.drop 1 httpVer'
    when (rest == "" || httpVer == "") $
        throwIO $ BadFirstLine $ B.unpack requestLine
    let (!path,!query) = S.breakByte 63 pathQuery -- '?'
        (!http,!ver)   = S.breakByte 47 httpVer -- '/'
    when (http /= "HTTP") $ throwIO NonHttp
    let !hv | ver == "/1.1" = H.http11
            | otherwise     = H.http10
    return $! (method,path,query,hv)

----------------------------------------------------------------

parseRequestLine0 :: ByteString
                 -> IO (H.Method
                       ,ByteString -- Path
                       ,ByteString -- Query
                       ,H.HttpVersion)
parseRequestLine0 s =
    case filter (not . S.null) $ S.splitWith (\c -> c == 32 || c == 9) s of  --  '
        (method':query:http'') -> do
            let !method = method'
                !http' = S.concat http''
                (!hfirst, !hsecond) = S.splitAt 5 http'
            if hfirst == "HTTP/"
               then let (!rpath, !qstring) = S.breakByte 63 query  -- '?'
                        !hv =
                            case hsecond of
                                "1.1" -> H.http11
                                _ -> H.http10
                    in return $! (method, rpath, qstring, hv)
               else throwIO NonHttp
        _ -> throwIO $ BadFirstLine $ B.unpack s