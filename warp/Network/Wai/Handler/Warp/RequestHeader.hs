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

----------------------------------------------------------------

parseHeaderLines :: [ByteString] -> IO (ByteString, ByteString, ByteString, H.HttpVersion, H.RequestHeaders)
parseHeaderLines [] = throwIO $ NotEnoughLines []
parseHeaderLines (firstLine:otherLines) = do
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let rpath = parseRpath rpath'
        hdr = map parseHeaderNoAttr otherLines
    return (method, rpath, gets, httpversion, hdr)

----------------------------------------------------------------

parseFirst :: ByteString
           -> IO (ByteString, ByteString, ByteString, H.HttpVersion)
parseFirst s =
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

parseHeaderNoAttr :: ByteString -> H.Header
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s -- ':'
        rest' = S.dropWhile (\c -> c == 32 || c == 9) $ S.drop 1 rest
     in (CI.mk k, rest')

----------------------------------------------------------------

parseRpath :: ByteString -> ByteString
parseRpath rpath'
  | S.null rpath' = "/"
  | "http://" `S.isPrefixOf` rpath' = snd $ S.breakByte 47 $ S.drop 7 rpath'
  | otherwise = rpath'
