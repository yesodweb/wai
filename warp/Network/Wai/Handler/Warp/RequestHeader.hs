{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.RequestHeader (
      parseHeaderLines
    , parseByteRanges
    ) where

import Control.Exception (throwIO)
import Control.Monad (when)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B (unpack)
import Data.ByteString.Internal (ByteString(..), memchr)
import qualified Data.CaseInsensitive as CI
import Data.Word (Word8)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, minusPtr, nullPtr)
import Foreign.Storable (peek)
import qualified Network.HTTP.Types as H
import Network.Wai.Handler.Warp.Types
import qualified Network.HTTP.Types.Header as HH
import qualified Data.Attoparsec.ByteString.Char8  as A8
import           Control.Applicative ((<|>), (<$>))
-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

parseHeaderLines :: [ByteString]
                 -> IO (H.Method
                       ,ByteString  --  Path
                       ,ByteString  --  Query
                       ,H.HttpVersion
                       ,H.RequestHeaders
                       )
parseHeaderLines [] = throwIO $ NotEnoughLines []
parseHeaderLines (firstLine:otherLines) = do
    (method, path', query, httpversion) <- parseRequestLine firstLine
    let path = parsePath path'
        hdr = map parseHeader otherLines
    return (method, path, query, httpversion, hdr)

----------------------------------------------------------------

-- |
--
-- >>> parseRequestLine "GET / HTTP/1.1"
-- ("GET","/","",HTTP/1.1)
-- >>> parseRequestLine "POST /cgi/search.cgi?key=foo HTTP/1.0"
-- ("POST","/cgi/search.cgi","?key=foo",HTTP/1.0)
-- >>> parseRequestLine "GET "
-- *** Exception: BadFirstLine "GET "
-- >>> parseRequestLine "GET /NotHTTP UNKNOWN/1.1"
-- *** Exception: NonHttp
parseRequestLine :: ByteString
                 -> IO (H.Method
                       ,ByteString -- Path
                       ,ByteString -- Query
                       ,H.HttpVersion)
parseRequestLine requestLine@(PS fptr off len) = withForeignPtr fptr $ \ptr -> do
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
-- >>> parsePath ""
-- "/"
-- >>> parsePath "http://example.com:8080/path"
-- "/path"
-- >>> parsePath "http://example.com"
-- "/"
-- >>> parsePath "/path"
-- "/path"

-- FIXME: parsePath "http://example.com" should be "/"?
parsePath :: ByteString -> ByteString
parsePath path
  | "http://" `S.isPrefixOf` path = ensureNonEmpty $ extractPath path
  | otherwise                     = ensureNonEmpty path
  where
    extractPath = snd . S.breakByte 47 . S.drop 7 -- 47 is '/'.
    ensureNonEmpty "" = "/"
    ensureNonEmpty p  = p

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
    let (k, rest) = S.breakByte 58 s -- ':'
        rest' = S.dropWhile (\c -> c == 32 || c == 9) $ S.drop 1 rest
     in (CI.mk k, rest')

byteRangesParser :: A8.Parser HH.ByteRanges
byteRangesParser = do
    _ <- A8.string "bytes="
    br <- range
    rest <- maybeMoreRanges 
    return $ br:rest
    where
        range = rangeFromTo <|> rangeSuffix
        rangeFromTo = do
            f <- A8.decimal
            _ <- A8.char '-'
            mt <- Just <$> A8.decimal <|> return Nothing
            
            return $ case mt of
                Just t -> HH.ByteRangeFromTo f t
                Nothing -> HH.ByteRangeFrom f
        rangeSuffix = do
            _ <- A8.char '-'
            s <- A8.decimal
            return $ HH.ByteRangeSuffix s
        maybeMoreRanges = moreRanges <|> return []
        moreRanges = do
            _ <- A8.char ','
            r <- range
            rest <- maybeMoreRanges
            return $ r:rest
       
parseByteRanges :: S.ByteString -> Maybe HH.ByteRanges
parseByteRanges bs = case A8.parseOnly byteRangesParser bs of
    Left _ -> Nothing
    Right br -> Just br
