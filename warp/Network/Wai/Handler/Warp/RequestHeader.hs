{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.RequestHeader (
    parseHeaderLines,
) where

import Control.Exception (throwIO)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as C8 (unpack)
import Data.ByteString.Internal (memchr)
import qualified Data.CaseInsensitive as CI
import Data.Word8
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, minusPtr, nullPtr, plusPtr)
import Foreign.Storable (peek)
import qualified Network.HTTP.Types as H

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
    hdr <- mapM parseHeader otherLines
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
-- >>> parseHeader "Content-Length : 47"
-- *** Exception: Warp: Invalid header field name: "Content-Length "
-- >>> parseHeader "X-Foo: a\rContent-Length: 47"
-- *** Exception: Warp: CR or NUL in a header field line
parseHeader :: ByteString -> IO H.Header
parseHeader s = do
    -- The line was ended by an LF, and the CR before it is already
    -- gone, so any CR left here is a bare one. RFC 9110 section 5.5
    -- asks for it to be refused or replaced: a recipient which takes a
    -- bare CR for a line terminator reads a field, and a framing field
    -- among them, where warp reads only data.
    when (hasControl s) $ throwIO BadHeaderLine
    -- RFC 9112 asks for 400 here, because a recipient which instead
    -- repairs the name reads a header where warp reads none: a
    -- Content-Length hidden that way is a request body warp does not
    -- know it is being sent.
    when (S.null k || not (S.all isTchar k)) $ throwIO $ BadFieldName k
    return (CI.mk k, rest')
  where
    (k, rest) = S.break (== _colon) s
    rest' = S.dropWhile isOWS $ S.drop 1 rest
    isOWS c = c == _space || c == _tab
    -- Two passes of memchr beat one pass of a lambda here by a wide
    -- margin, and 'S.elem' is the way to ask for memchr: measured over
    -- a browser's request headers, this takes a fifth of the time that
    -- @S.any (_cr ==)@ does and a fifteenth of the time that one
    -- @S.any@ with a two-way lambda does.
    hasControl bs = S.elem _cr bs || S.elem _nul bs

-- | The characters RFC 9110 allows in a header field name.
isTchar :: Word8 -> Bool
isTchar w =
    isAsciiUpper w
        || isAsciiLower w
        || isDigit w
        || w
            `elem` [ _exclam
                   , _numbersign
                   , _dollar
                   , _percent
                   , _ampersand
                   , _quotesingle
                   , _asterisk
                   , _plus
                   , _hyphen
                   , _period
                   , _circum
                   , _underscore
                   , _grave
                   , _bar
                   , _tilde
                   ]
