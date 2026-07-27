{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.Header (
    IndexedHeader,
    IndexedRequestHeader (..),
    IndexedResponseHeader,
    (!),
    indexRequestHeader,
    defaultIndexRequestHeader,
    ResponseHeaderIndex (..),
    indexResponseHeader,
) where

import Data.Array (Array)
import qualified Data.Array as A ((!))
import Data.Array.ST
import qualified Data.ByteString as BS
import Data.CaseInsensitive (foldedCase)
import Network.HTTP.Types

import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | Array for a set of HTTP headers.
newtype IndexedHeader a = IxHeader (Array Int (Maybe HeaderValue))

type IndexedResponseHeader = IndexedHeader ResponseHeaderIndex

-- | Safer way to lookup 'IndexedHeader' values
(!) :: Enum a => IndexedHeader a -> a -> Maybe HeaderValue
(IxHeader ixHdr) ! ix = ixHdr A.! fromEnum ix

----------------------------------------------------------------

-- | Strict record of the request headers that Warp inspects,
--   one field per header.
data IndexedRequestHeader = IndexedRequestHeader
    { reqidxContentLength :: Maybe HeaderValue
    , reqidxTransferEncoding :: Maybe HeaderValue
    , reqidxExpect :: Maybe HeaderValue
    , reqidxConnection :: Maybe HeaderValue
    , reqidxRange :: Maybe HeaderValue
    , reqidxHost :: Maybe HeaderValue
    , reqidxIfModifiedSince :: Maybe HeaderValue
    , reqidxIfUnmodifiedSince :: Maybe HeaderValue
    , reqidxIfRange :: Maybe HeaderValue
    , reqidxReferer :: Maybe HeaderValue
    , reqidxUserAgent :: Maybe HeaderValue
    , reqidxIfMatch :: Maybe HeaderValue
    , reqidxIfNoneMatch :: Maybe HeaderValue
    }

indexRequestHeader :: RequestHeaders -> IndexedRequestHeader
indexRequestHeader = foldl' insert defaultIndexRequestHeader
  where
    insert ix (key, val) = case BS.length bs of
        4 | bs == "host" -> ix{reqidxHost = Just val}
        5 | bs == "range" -> ix{reqidxRange = Just val}
        6 | bs == "expect" -> ix{reqidxExpect = Just val}
        7 | bs == "referer" -> ix{reqidxReferer = Just val}
        8
            | bs == "if-range" -> ix{reqidxIfRange = Just val}
            | bs == "if-match" -> ix{reqidxIfMatch = Just val}
        10
            | bs == "user-agent" -> ix{reqidxUserAgent = Just val}
            | bs == "connection" -> ix{reqidxConnection = Just val}
        13 | bs == "if-none-match" -> ix{reqidxIfNoneMatch = Just val}
        14 | bs == "content-length" -> ix{reqidxContentLength = Just val}
        17
            | bs == "transfer-encoding" -> ix{reqidxTransferEncoding = Just val}
            | bs == "if-modified-since" -> ix{reqidxIfModifiedSince = Just val}
        19 | bs == "if-unmodified-since" -> ix{reqidxIfUnmodifiedSince = Just val}
        _ -> ix
      where
        bs = foldedCase key

-- | 'IndexedRequestHeader' with no headers set.
defaultIndexRequestHeader :: IndexedRequestHeader
defaultIndexRequestHeader =
    IndexedRequestHeader
        { reqidxContentLength = Nothing
        , reqidxTransferEncoding = Nothing
        , reqidxExpect = Nothing
        , reqidxConnection = Nothing
        , reqidxRange = Nothing
        , reqidxHost = Nothing
        , reqidxIfModifiedSince = Nothing
        , reqidxIfUnmodifiedSince = Nothing
        , reqidxIfRange = Nothing
        , reqidxReferer = Nothing
        , reqidxUserAgent = Nothing
        , reqidxIfMatch = Nothing
        , reqidxIfNoneMatch = Nothing
        }

----------------------------------------------------------------

indexResponseHeader :: ResponseHeaders -> IndexedHeader ResponseHeaderIndex
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
