{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.Header where

import Data.Array
import Data.Array.ST
import qualified Data.ByteString as BS
import Data.CaseInsensitive (foldedCase)
import Network.HTTP.Types

import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | Array for a set of HTTP headers.
type IndexedHeader = Array Int (Maybe HeaderValue)

----------------------------------------------------------------

indexRequestHeader :: RequestHeaders -> IndexedHeader
indexRequestHeader hdr = traverseHeader hdr requestMaxIndex requestKeyIndex

data RequestHeaderIndex
    = ReqContentLength
    | ReqTransferEncoding
    | ReqExpect
    | ReqConnection
    | ReqRange
    | ReqHost
    | ReqIfModifiedSince
    | ReqIfUnmodifiedSince
    | ReqIfRange
    | ReqReferer
    | ReqUserAgent
    | ReqIfMatch
    | ReqIfNoneMatch
    deriving (Enum, Bounded)

-- | The size for 'IndexedHeader' for HTTP Request.
--   From 0 to this corresponds to:
--
-- - \"Content-Length\"
-- - \"Transfer-Encoding\"
-- - \"Expect\"
-- - \"Connection\"
-- - \"Range\"
-- - \"Host\"
-- - \"If-Modified-Since\"
-- - \"If-Unmodified-Since\"
-- - \"If-Range\"
-- - \"Referer\"
-- - \"User-Agent\"
-- - \"If-Match\"
-- - \"If-None-Match\"
requestMaxIndex :: Int
requestMaxIndex = fromEnum (maxBound :: RequestHeaderIndex)

requestKeyIndex :: HeaderName -> Int
requestKeyIndex hn = case BS.length bs of
    4 | bs == "host" -> fromEnum ReqHost
    5 | bs == "range" -> fromEnum ReqRange
    6 | bs == "expect" -> fromEnum ReqExpect
    7 | bs == "referer" -> fromEnum ReqReferer
    8
        | bs == "if-range" -> fromEnum ReqIfRange
        | bs == "if-match" -> fromEnum ReqIfMatch
    10
        | bs == "user-agent" -> fromEnum ReqUserAgent
        | bs == "connection" -> fromEnum ReqConnection
    13 | bs == "if-none-match" -> fromEnum ReqIfNoneMatch
    14 | bs == "content-length" -> fromEnum ReqContentLength
    17
        | bs == "transfer-encoding" -> fromEnum ReqTransferEncoding
        | bs == "if-modified-since" -> fromEnum ReqIfModifiedSince
    19 | bs == "if-unmodified-since" -> fromEnum ReqIfUnmodifiedSince
    _ -> -1
  where
    bs = foldedCase hn

defaultIndexRequestHeader :: IndexedHeader
defaultIndexRequestHeader = array (0, requestMaxIndex) [(i, Nothing) | i <- [0 .. requestMaxIndex]]

----------------------------------------------------------------

indexResponseHeader :: ResponseHeaders -> IndexedHeader
indexResponseHeader hdr = traverseHeader hdr responseMaxIndex responseKeyIndex

data ResponseHeaderIndex
    = ResContentLength
    | ResServer
    | ResDate
    | ResLastModified
    deriving (Enum, Bounded)

-- | The size for 'IndexedHeader' for HTTP Response.
responseMaxIndex :: Int
responseMaxIndex = fromEnum (maxBound :: ResponseHeaderIndex)

responseKeyIndex :: HeaderName -> Int
responseKeyIndex hn = case BS.length bs of
    4 | bs == "date" -> fromEnum ResDate
    6 | bs == "server" -> fromEnum ResServer
    13 | bs == "last-modified" -> fromEnum ResLastModified
    14 | bs == "content-length" -> fromEnum ResContentLength
    _ -> -1
  where
    bs = foldedCase hn

----------------------------------------------------------------

traverseHeader :: [Header] -> Int -> (HeaderName -> Int) -> IndexedHeader
traverseHeader hdr maxidx getIndex = runSTArray $ do
    arr <- newArray (0, maxidx) Nothing
    mapM_ (insert arr) hdr
    return arr
  where
    insert arr (key, val)
        | idx == -1 = return ()
        | otherwise = writeArray arr idx (Just val)
      where
        idx = getIndex key
