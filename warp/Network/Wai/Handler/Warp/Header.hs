{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.Header (
    IndexedHeader,
    IndexedRequestHeader,
    IndexedResponseHeader (..),
    (!),
    RequestHeaderIndex (..),
    indexRequestHeader,
    requestMaxIndex,
    defaultIndexRequestHeader,
    indexResponseHeader,
) where

import Data.Array (Array, array)
import qualified Data.Array as A ((!))
import Data.Array.ST
import qualified Data.ByteString as BS
import Data.CaseInsensitive (foldedCase)
import Network.HTTP.Types

import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | Array for a set of HTTP headers.
newtype IndexedHeader a = IxHeader (Array Int (Maybe HeaderValue))

type IndexedRequestHeader = IndexedHeader RequestHeaderIndex

-- | Safer way to lookup 'IndexedHeader' values
(!) :: Enum a => IndexedHeader a -> a -> Maybe HeaderValue
(IxHeader ixHdr) ! ix = ixHdr A.! fromEnum ix

----------------------------------------------------------------

indexRequestHeader :: RequestHeaders -> IndexedHeader RequestHeaderIndex
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

defaultIndexRequestHeader :: IndexedHeader RequestHeaderIndex
defaultIndexRequestHeader =
    IxHeader $
        array (0, requestMaxIndex) [(i, Nothing) | i <- [0 .. requestMaxIndex]]

----------------------------------------------------------------

-- | Index for the response headers Warp itself consults.
--   Only these four headers are ever looked up on the response side,
--   so a flat record built in a single traversal beats a boxed array.
--   The fields are strict via StrictData.
data IndexedResponseHeader = IndexedResponseHeader
    { resContentLength :: Maybe HeaderValue
    , resServer :: Maybe HeaderValue
    , resDate :: Maybe HeaderValue
    , resLastModified :: Maybe HeaderValue
    }

indexResponseHeader :: ResponseHeaders -> IndexedResponseHeader
indexResponseHeader = go emptyIndexedResponseHeader
  where
    go ix [] = ix
    go ix ((key, val) : rest) = go (insert ix key val) rest
    -- Like 'traverseHeader', a later duplicate wins.
    insert ix key val = case BS.length bs of
        4 | bs == "date" -> ix{resDate = Just val}
        6 | bs == "server" -> ix{resServer = Just val}
        13 | bs == "last-modified" -> ix{resLastModified = Just val}
        14 | bs == "content-length" -> ix{resContentLength = Just val}
        _ -> ix
      where
        bs = foldedCase key

emptyIndexedResponseHeader :: IndexedResponseHeader
emptyIndexedResponseHeader =
    IndexedResponseHeader
        { resContentLength = Nothing
        , resServer = Nothing
        , resDate = Nothing
        , resLastModified = Nothing
        }

----------------------------------------------------------------

traverseHeader :: [Header] -> Int -> (HeaderName -> Int) -> IndexedHeader a
traverseHeader hdr maxidx getIndex = IxHeader $ runSTArray $ do
    arr <- newArray (0, maxidx) Nothing
    mapM_ (insert arr) hdr
    return arr
  where
    insert arr (key, val)
        | idx == -1 = return ()
        | otherwise = writeArray arr idx (Just val)
      where
        idx = getIndex key
