{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.RequestHeader (parseHeaderLines) where

import Control.Exception.Lifted (throwIO)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.CaseInsensitive as CI
import qualified Network.HTTP.Types as H
import Network.Wai.Handler.Warp.Types
import Prelude hiding (lines)

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
-- >>> parseRequestLine "POST  /cgi/search.cgi?key=foo  HTTP/1.0"
-- ("POST","/cgi/search.cgi","?key=foo",HTTP/1.0)
-- >>> parseRequestLine "Broken"
-- *** Exception: BadFirstLine "Broken"
parseRequestLine :: ByteString
           -> IO (ByteString, ByteString, ByteString, H.HttpVersion)
parseRequestLine s =
    case filter (not . S.null) $ S.splitWith (\c -> c == 32 || c == 9) s of  -- ' '
        (method:query:http'') -> do
            let http' = S.concat http''
                (hfirst, hsecond) = S.splitAt 5 http'
            if hfirst == "HTTP/"
               then let (rpath, qstring) = S.breakByte 63 query  -- '?'
                        hv =
                            case hsecond of
                                "1.1" -> H.http11
                                _ -> H.http10
                    in return (method, rpath, qstring, hv)
               else throwIO NonHttp
        _ -> throwIO $ BadFirstLine $ B.unpack s

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
  | otherwise                     = ensureNonEmpty $ path
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
