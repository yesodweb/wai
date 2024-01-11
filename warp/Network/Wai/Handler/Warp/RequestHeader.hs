{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.RequestHeader (
    parseHeaderLines,
) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8 (unpack)
import Data.ByteString.Internal (memchr)
import qualified Data.CaseInsensitive as CI
import Data.Word8
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable (peek)
import qualified Network.HTTP.Types as H
import UnliftIO (throwIO)

import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

parseHeaderLines
    :: [ByteString]
    -> IO
        ( H.Method
        , ByteString --  Path
        , ByteString --  Path, parsed
        , ByteString --  Query
        , H.HttpVersion
        , H.RequestHeaders
        )
parseHeaderLines [] = throwIO $ NotEnoughLines []
parseHeaderLines (firstLine : otherLines) = do
    (method, path', query, httpversion) <- parseRequestLine firstLine
    let path = H.extractPath path'
        hdr = map parseHeader otherLines
    return (method, path', path, query, httpversion, hdr)

----------------------------------------------------------------

-- |
--
-- >>> parseRequestLine "GET / HTTP/1.1"
-- ("GET","/","",HTTP/1.1)
-- >>> parseRequestLine "POST /cgi/search.cgi?key=foo HTTP/1.0"
-- ("POST","/cgi/search.cgi","?key=foo",HTTP/1.0)
-- >>> parseRequestLine "GET "
-- *** Exception: Warp: Invalid first line of request: "GET "
-- >>> parseRequestLine "GET /NotHTTP UNKNOWN/1.1"
-- *** Exception: Warp: Request line specified a non-HTTP request
-- >>> parseRequestLine "PRI * HTTP/2.0"
-- ("PRI","*","",HTTP/2.0)
parseRequestLine
    :: ByteString
    -> IO
        ( H.Method
        , ByteString -- Path
        , ByteString -- Query
        , H.HttpVersion
        )
parseRequestLine requestLine@(PS fptr off len) = withForeignPtr fptr $ \ptr -> do
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
    baderr = BadFirstLine $ C8.unpack requestLine
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
        major <- peek (httpptr `plusPtr` 5) :: IO Word8
        minor <- peek (httpptr `plusPtr` 7) :: IO Word8
        let version
                | major == _1 = if minor == _1 then H.http11 else H.http10
                | major == _2 && minor == _0 = H.http20
                | otherwise = H.http10
        return version
    bs ptr p0 p1 = PS fptr o l
      where
        o = p0 `minusPtr` ptr
        l = p1 `minusPtr` p0

----------------------------------------------------------------

-- |
--
-- >>> parseHeader "Content-Length:47"
-- ("Content-Length","47")
-- >>> parseHeader "Accept-Ranges: bytes"
-- ("Accept-Ranges","bytes")
-- >>> parseHeader "Host:  example.com:8080"
-- ("Host","example.com:8080")
-- >>> parseHeader "NoSemiColon"
-- ("NoSemiColon","")
parseHeader :: ByteString -> H.Header
parseHeader s =
    let (k, rest) = S.break (== _colon) s
        rest' = S.dropWhile (\c -> c == _space || c == _tab) $ S.drop 1 rest
     in (CI.mk k, rest')
