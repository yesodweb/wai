{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.Header (
    IndexedHeader,
    IndexedRequestHeader (..),
    ResponseHeaderPresence (..),
    (!),
    indexRequestHeader,
    defaultIndexRequestHeader,
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
