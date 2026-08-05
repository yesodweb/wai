{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.Header (
    IndexedHeader,
    IndexedRequestHeader,
    ResponseHeaderPresence (..),
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

-- | Presence of the response headers Warp itself consults.
--   Only these four headers are ever looked up on the response side, and
--   only their presence, never their value, so a flat record of strict
--   'Bool's built in a single traversal beats a boxed array.
data ResponseHeaderPresence = ResponseHeaderPresence
    { hasContentLength :: Bool
    , hasServer :: Bool
    , hasDate :: Bool
    , hasLastModified :: Bool
    }

indexResponseHeader :: ResponseHeaders -> ResponseHeaderPresence
indexResponseHeader = go emptyResponseHeaderPresence
  where
    go ix [] = ix
    go ix ((key, _) : rest) = go (insert ix key) rest
    insert ix key = case BS.length bs of
        4 | bs == "date" -> ix{hasDate = True}
        6 | bs == "server" -> ix{hasServer = True}
        13 | bs == "last-modified" -> ix{hasLastModified = True}
        14 | bs == "content-length" -> ix{hasContentLength = True}
        _ -> ix
      where
        bs = foldedCase key

emptyResponseHeaderPresence :: ResponseHeaderPresence
emptyResponseHeaderPresence =
    ResponseHeaderPresence
        { hasContentLength = False
        , hasServer = False
        , hasDate = False
        , hasLastModified = False
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
