{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Exception (throw, throwIO)
import Control.Monad
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B (pack, unpack)
import Data.ByteString.Internal
import Data.IORef (atomicModifyIORef', newIORef)
import qualified Data.List as L
import Data.Word8
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable
import qualified Network.HTTP.Types as H
import Prelude hiding (lines)

import Network.Wai.Handler.Warp.Request (FirstRequest (..), headerLines)
import Network.Wai.Handler.Warp.Types

import Criterion.Main

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

main :: IO ()
main = do
    let requestLine1 = "GET http://www.example.com HTTP/1.1"
    let requestLine2 = "GET http://www.example.com/cgi-path/search.cgi?key=parser HTTP/1.0"
    defaultMain
        [ bgroup
            "requestLine1"
            [ bench "parseRequestLine3" $ whnf parseRequestLine3 requestLine1
            , bench "parseRequestLine2" $ whnfIO $ parseRequestLine2 requestLine1
            , bench "parseRequestLine1" $ whnfIO $ parseRequestLine1 requestLine1
            , bench "parseRequestLine0" $ whnfIO $ parseRequestLine0 requestLine1
            ]
        , bgroup
            "requestLine2"
            [ bench "parseRequestLine3" $ whnf parseRequestLine3 requestLine2
            , bench "parseRequestLine2" $ whnfIO $ parseRequestLine2 requestLine2
            , bench "parseRequestLine1" $ whnfIO $ parseRequestLine1 requestLine2
            , bench "parseRequestLine0" $ whnfIO $ parseRequestLine0 requestLine2
            ]
        , bgroup
            "parsing request"
            [ bench "new parsing 4" $ whnfAppIO testIt (chunkRequest 4)
            , bench "new parsing 10" $ whnfAppIO testIt (chunkRequest 10)
            , bench "new parsing 25" $ whnfAppIO testIt (chunkRequest 25)
            , bench "new parsing 100" $ whnfAppIO testIt (chunkRequest 100)
            ]
        ]
  where
    testIt req = producer req >>= headerLines 800 FirstRequest

----------------------------------------------------------------

-- |
--
-- >>> parseRequestLine3 "GET / HTTP/1.1"
-- ("GET","/","",HTTP/1.1)
-- >>> parseRequestLine3 "POST /cgi/search.cgi?key=foo HTTP/1.0"
-- ("POST","/cgi/search.cgi","?key=foo",HTTP/1.0)
-- >>> parseRequestLine3 "GET "
-- *** Exception: BadFirstLine "GET "
-- >>> parseRequestLine3 "GET /NotHTTP UNKNOWN/1.1"
-- *** Exception: NonHttp
parseRequestLine3
    :: ByteString
    -> ( H.Method
       , ByteString -- Path
       , ByteString -- Query
       , H.HttpVersion
       )
parseRequestLine3 requestLine = ret
  where
    (!method, !rest) = S.break (== _space) requestLine
    (!pathQuery, !httpVer')
        | rest == "" = throw badmsg
        | otherwise = S.break (== _space) (S.drop 1 rest)
    (!path, !query) = S.break (== _question) pathQuery
    !httpVer = S.drop 1 httpVer'
    (!http, !ver)
        | httpVer == "" = throw badmsg
        | otherwise = S.break (== _slash) httpVer
    !hv
        | http /= "HTTP" = throw NonHttp
        | ver == "/1.1" = H.http11
        | otherwise = H.http10
    !ret = (method, path, query, hv)
    badmsg = BadFirstLine $ B.unpack requestLine

----------------------------------------------------------------

-- |
--
-- >>> parseRequestLine2 "GET / HTTP/1.1"
-- ("GET","/","",HTTP/1.1)
-- >>> parseRequestLine2 "POST /cgi/search.cgi?key=foo HTTP/1.0"
-- ("POST","/cgi/search.cgi","?key=foo",HTTP/1.0)
-- >>> parseRequestLine2 "GET "
-- *** Exception: BadFirstLine "GET "
-- >>> parseRequestLine2 "GET /NotHTTP UNKNOWN/1.1"
-- *** Exception: NonHttp
parseRequestLine2
    :: ByteString
    -> IO
        ( H.Method
        , ByteString -- Path
        , ByteString -- Query
        , H.HttpVersion
        )
parseRequestLine2 requestLine@(PS fptr off len) = withForeignPtr fptr $ \ptr -> do
    when (len < 14) $ throwIO baderr
    let methodptr = ptr `plusPtr` off
        limptr = methodptr `plusPtr` len
        lim0 = fromIntegral len

    pathptr0 <- memchr methodptr _space lim0
    when (pathptr0 == nullPtr || (limptr `minusPtr` pathptr0) < 11) $
        throwIO baderr
    let pathptr = pathptr0 `plusPtr` 1
        lim1 = fromIntegral (limptr `minusPtr` pathptr0)

    httpptr0 <- memchr pathptr _space lim1
    when (httpptr0 == nullPtr || (limptr `minusPtr` httpptr0) < 9) $
        throwIO baderr
    let httpptr = httpptr0 `plusPtr` 1
        lim2 = fromIntegral (httpptr0 `minusPtr` pathptr)

    checkHTTP httpptr
    !hv <- httpVersion httpptr
    queryptr <- memchr pathptr _question lim2
    let !method = bs ptr methodptr pathptr0
        !path
            | queryptr == nullPtr = bs ptr pathptr httpptr0
            | otherwise = bs ptr pathptr queryptr
        !query
            | queryptr == nullPtr = S.empty
            | otherwise = bs ptr queryptr httpptr0

    return (method, path, query, hv)
  where
    baderr = BadFirstLine $ B.unpack requestLine
    check :: Ptr Word8 -> Int -> Word8 -> IO ()
    check p n w = do
        w0 <- peek $ p `plusPtr` n
        when (w0 /= w) $ throwIO NonHttp
    checkHTTP httpptr = do
        check httpptr 0 _H
        check httpptr 1 _T
        check httpptr 2 _T
        check httpptr 3 _P
        check httpptr 4 _slash
        check httpptr 6 _period
    httpVersion httpptr = do
        major <- peek $ httpptr `plusPtr` 5
        minor <- peek $ httpptr `plusPtr` 7
        if major == _1 && minor == _1
            then return H.http11
            else return H.http10
    bs ptr p0 p1 = PS fptr o l
      where
        o = p0 `minusPtr` ptr
        l = p1 `minusPtr` p0

----------------------------------------------------------------

-- |
--
-- >>> parseRequestLine1 "GET / HTTP/1.1"
-- ("GET","/","",HTTP/1.1)
-- >>> parseRequestLine1 "POST /cgi/search.cgi?key=foo HTTP/1.0"
-- ("POST","/cgi/search.cgi","?key=foo",HTTP/1.0)
-- >>> parseRequestLine1 "GET "
-- *** Exception: BadFirstLine "GET "
-- >>> parseRequestLine1 "GET /NotHTTP UNKNOWN/1.1"
-- *** Exception: NonHttp
parseRequestLine1
    :: ByteString
    -> IO
        ( H.Method
        , ByteString -- Path
        , ByteString -- Query
        , H.HttpVersion
        )
parseRequestLine1 requestLine = do
    let (!method, !rest) = S.break (== _space) requestLine
        (!pathQuery, !httpVer') = S.break (== _space) (S.drop 1 rest)
        !httpVer = S.drop 1 httpVer'
    when (rest == "" || httpVer == "") $
        throwIO $
            BadFirstLine $
                B.unpack requestLine
    let (!path, !query) = S.break (== _question) pathQuery
        (!http, !ver) = S.break (== _slash) httpVer
    when (http /= "HTTP") $ throwIO NonHttp
    let !hv
            | ver == "/1.1" = H.http11
            | otherwise = H.http10
    return $! (method, path, query, hv)

----------------------------------------------------------------

-- |
--
-- >>> parseRequestLine0 "GET / HTTP/1.1"
-- ("GET","/","",HTTP/1.1)
-- >>> parseRequestLine0 "POST /cgi/search.cgi?key=foo HTTP/1.0"
-- ("POST","/cgi/search.cgi","?key=foo",HTTP/1.0)
-- >>> parseRequestLine0 "GET "
-- *** Exception: BadFirstLine "GET "
-- >>> parseRequestLine0 "GET /NotHTTP UNKNOWN/1.1"
-- *** Exception: NonHttp
parseRequestLine0
    :: ByteString
    -> IO
        ( H.Method
        , ByteString -- Path
        , ByteString -- Query
        , H.HttpVersion
        )
parseRequestLine0 s =
    case filter (not . S.null) $ S.splitWith (\c -> c == _space || c == _tab) s of
        (method' : query : http'') -> do
            let !method = method'
                !http' = S.concat http''
                (!hfirst, !hsecond) = S.splitAt 5 http'
            if hfirst == "HTTP/"
                then
                    let (!rpath, !qstring) = S.break (== _question) query
                        !hv =
                            case hsecond of
                                "1.1" -> H.http11
                                _ -> H.http10
                     in return $! (method, rpath, qstring, hv)
                else throwIO NonHttp
        _ -> throwIO $ BadFirstLine $ B.unpack s

producer :: [ByteString] -> IO Source
producer a = do
    ref <- newIORef a
    mkSource $
        atomicModifyIORef' ref $ \case
            [] -> ([], S.empty)
            b : bs -> (bs, b)

chunkRequest :: Int -> [ByteString]
chunkRequest chunkAmount =
    go basicRequest
  where
    len = L.length basicRequest
    chunkSize = (len `div` chunkAmount) + 1
    go [] = []
    go xs =
        let (a, b) = L.splitAt chunkSize xs
         in B.pack a : go b

-- Random google search request
basicRequest :: String
basicRequest =
    mconcat
        [ "GET /search?q=test&sca_esv=600090652&source=hp&uact=5&oq=test&sclient=gws-wiz HTTP/3\r\n"
        , "Host: www.google.com\r\n"
        , "User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:121.0) Gecko/20100101 Firefox/121.0\r\n"
        , "Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8\r\n"
        , "Accept-Language: en-US,en;q=0.5\r\n"
        , "Accept-Encoding: gzip, deflate, br\r\n"
        , "Referer: https://www.google.com/\r\n"
        , "Alt-Used: www.google.com\r\n"
        , "Connection: keep-alive\r\n"
        , "Cookie: CONSENT=PENDING+252\r\n"
        , "Upgrade-Insecure-Requests: 1\r\n"
        , "Sec-Fetch-Dest: document\r\n"
        , "Sec-Fetch-Mode: navigate\r\n"
        , "Sec-Fetch-Site: same-origin\r\n"
        , "Sec-Fetch-User: ?1\r\n"
        , "TE: trailers\r\n\r\n"
        ]
