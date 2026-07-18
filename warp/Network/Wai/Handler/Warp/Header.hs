{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.Header (
    IndexedRequestHeader (..),
    IndexedResponseHeader (..),
    indexRequestHeader,
    defaultIndexRequestHeader,
    indexResponseHeader,
) where

import qualified Data.ByteString as BS
import Data.CaseInsensitive (foldedCase)
import Network.HTTP.Types

import Network.Wai.Handler.Warp.Types

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

indexRequestHeader :: RequestHeaders -> IndexedRequestHeader
indexRequestHeader = foldl' insert defaultIndexRequestHeader
  where
    insert ix (key, val) = case requestKeyIndex key of
        Nothing -> ix
        Just ReqContentLength -> ix{reqidxContentLength = Just val}
        Just ReqTransferEncoding -> ix{reqidxTransferEncoding = Just val}
        Just ReqExpect -> ix{reqidxExpect = Just val}
        Just ReqConnection -> ix{reqidxConnection = Just val}
        Just ReqRange -> ix{reqidxRange = Just val}
        Just ReqHost -> ix{reqidxHost = Just val}
        Just ReqIfModifiedSince -> ix{reqidxIfModifiedSince = Just val}
        Just ReqIfUnmodifiedSince -> ix{reqidxIfUnmodifiedSince = Just val}
        Just ReqIfRange -> ix{reqidxIfRange = Just val}
        Just ReqReferer -> ix{reqidxReferer = Just val}
        Just ReqUserAgent -> ix{reqidxUserAgent = Just val}
        Just ReqIfMatch -> ix{reqidxIfMatch = Just val}
        Just ReqIfNoneMatch -> ix{reqidxIfNoneMatch = Just val}

requestKeyIndex :: HeaderName -> Maybe RequestHeaderIndex
requestKeyIndex hn = case BS.length bs of
    4 | bs == "host" -> Just ReqHost
    5 | bs == "range" -> Just ReqRange
    6 | bs == "expect" -> Just ReqExpect
    7 | bs == "referer" -> Just ReqReferer
    8
        | bs == "if-range" -> Just ReqIfRange
        | bs == "if-match" -> Just ReqIfMatch
    10
        | bs == "user-agent" -> Just ReqUserAgent
        | bs == "connection" -> Just ReqConnection
    13 | bs == "if-none-match" -> Just ReqIfNoneMatch
    14 | bs == "content-length" -> Just ReqContentLength
    17
        | bs == "transfer-encoding" -> Just ReqTransferEncoding
        | bs == "if-modified-since" -> Just ReqIfModifiedSince
    19 | bs == "if-unmodified-since" -> Just ReqIfUnmodifiedSince
    _ -> Nothing
  where
    bs = foldedCase hn

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
    -- A later duplicate wins.
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
