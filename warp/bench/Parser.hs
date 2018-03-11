{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Main where

import Control.Exception (throwIO, throw)
import Control.Monad
import qualified Data.ByteString as S
--import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Network.HTTP.Types as H
import Network.Wai.Handler.Warp.Types
import Prelude hiding (lines)

import Data.ByteString.Internal
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Foreign.Storable

#if MIN_VERSION_gauge(0, 2, 0)
import Gauge
#else
import Gauge.Main
#endif

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

main :: IO ()
main = do
    let requestLine1 = "GET http://www.example.com HTTP/1.1"
    let requestLine2 = "GET http://www.example.com/cgi-path/search.cgi?key=parser HTTP/1.0"
    defaultMain [
        bgroup "requestLine1" [
             bench "parseRequestLine3" $ whnf parseRequestLine3 requestLine1
           , bench "parseRequestLine2" $ whnfIO $ parseRequestLine2 requestLine1
           , bench "parseRequestLine1" $ whnfIO $ parseRequestLine1 requestLine1
           , bench "parseRequestLine0" $ whnfIO $ parseRequestLine0 requestLine1
           ]
      , bgroup "requestLine2" [
             bench "parseRequestLine3" $ whnf parseRequestLine3 requestLine2
           , bench "parseRequestLine2" $ whnfIO $ parseRequestLine2 requestLine2
           , bench "parseRequestLine1" $ whnfIO $ parseRequestLine1 requestLine2
           , bench "parseRequestLine0" $ whnfIO $ parseRequestLine0 requestLine2
           ]
      ]

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
parseRequestLine3 :: ByteString
                  -> (H.Method
                     ,ByteString -- Path
                     ,ByteString -- Query
                     ,H.HttpVersion)
parseRequestLine3 requestLine = ret
  where
    (!method,!rest) = S.break (== 32) requestLine -- ' '
    (!pathQuery,!httpVer')
      | rest == "" = throw badmsg
      | otherwise  = S.break (== 32) (S.drop 1 rest) -- ' '
    (!path,!query) = S.break (== 63) pathQuery -- '?'
    !httpVer = S.drop 1 httpVer'
    (!http,!ver)
      | httpVer == "" = throw badmsg
      | otherwise     = S.break (== 47) httpVer -- '/'
    !hv | http /= "HTTP" = throw NonHttp
        | ver == "/1.1"  = H.http11
        | otherwise      = H.http10
    !ret = (method,path,query,hv)
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
parseRequestLine2 :: ByteString
                  -> IO (H.Method
                        ,ByteString -- Path
                        ,ByteString -- Query
                        ,H.HttpVersion)
parseRequestLine2 requestLine@(PS fptr off len) = withForeignPtr fptr $ \ptr -> do
    when (len < 14) $ throwIO baderr
    let methodptr = ptr `plusPtr` off
        limptr = methodptr `plusPtr` len
        lim0 = fromIntegral len

    pathptr0 <- memchr methodptr 32 lim0 -- ' '
    when (pathptr0 == nullPtr || (limptr `minusPtr` pathptr0) < 11) $
        throwIO baderr
    let pathptr = pathptr0 `plusPtr` 1
        lim1 = fromIntegral (limptr `minusPtr` pathptr0)

    httpptr0 <- memchr pathptr 32 lim1 -- ' '
    when (httpptr0 == nullPtr || (limptr `minusPtr` httpptr0) < 9) $
        throwIO baderr
    let httpptr = httpptr0 `plusPtr` 1
        lim2 = fromIntegral (httpptr0 `minusPtr` pathptr)

    checkHTTP httpptr
    !hv <- httpVersion httpptr
    queryptr <- memchr pathptr 63 lim2 -- '?'

    let !method = bs ptr methodptr pathptr0
        !path
          | queryptr == nullPtr = bs ptr pathptr httpptr0
          | otherwise           = bs ptr pathptr queryptr
        !query
          | queryptr == nullPtr = S.empty
          | otherwise           = bs ptr queryptr httpptr0

    return (method,path,query,hv)
  where
    baderr = BadFirstLine $ B.unpack requestLine
    check :: Ptr Word8 -> Int -> Word8 -> IO ()
    check p n w = do
        w0 <- peek $ p `plusPtr` n
        when (w0 /= w) $ throwIO NonHttp
    checkHTTP httpptr = do
        check httpptr 0 72 -- 'H'
        check httpptr 1 84 -- 'T'
        check httpptr 2 84 -- 'T'
        check httpptr 3 80 -- 'P'
        check httpptr 4 47 -- '/'
        check httpptr 6 46 -- '.'
    httpVersion httpptr = do
        major <- peek $ httpptr `plusPtr` 5
        minor <- peek $ httpptr `plusPtr` 7
        if major == (49 :: Word8) && minor == (49 :: Word8) then
            return H.http11
          else
            return H.http10
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
parseRequestLine1 :: ByteString
                  -> IO (H.Method
                        ,ByteString -- Path
                        ,ByteString -- Query
                        ,H.HttpVersion)
parseRequestLine1 requestLine = do
    let (!method,!rest) = S.break (== 32) requestLine -- ' '
        (!pathQuery,!httpVer') = S.break (== 32) (S.drop 1 rest) -- ' '
        !httpVer = S.drop 1 httpVer'
    when (rest == "" || httpVer == "") $
        throwIO $ BadFirstLine $ B.unpack requestLine
    let (!path,!query) = S.break (== 63) pathQuery -- '?'
        (!http,!ver)   = S.break (== 47) httpVer -- '/'
    when (http /= "HTTP") $ throwIO NonHttp
    let !hv | ver == "/1.1" = H.http11
            | otherwise     = H.http10
    return $! (method,path,query,hv)

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
               then let (!rpath, !qstring) = S.break (== 63) query  -- '?'
                        !hv =
                            case hsecond of
                                "1.1" -> H.http11
                                _ -> H.http10
                    in return $! (method, rpath, qstring, hv)
               else throwIO NonHttp
        _ -> throwIO $ BadFirstLine $ B.unpack s
