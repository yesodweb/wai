{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

module Network.Wai.Handler.Warp.Header where

import Data.Array
import Data.Array.ST
import Network.HTTP.Types
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | Array for a set of HTTP headers.
type IndexedHeader = Array Int (Maybe HeaderValue)

----------------------------------------------------------------

indexRequestHeader :: RequestHeaders -> IndexedHeader
indexRequestHeader hdr = traverseHeader hdr requestMaxIndex requestKeyIndex

data RequestHeaderIndex = ReqContentLength
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
                        deriving (Enum,Bounded)

-- | The size for 'IndexedHeader' for HTTP Request.
--   From 0 to this corresponds to \"Content-Length\", \"Transfer-Encoding\",
--   \"Expect\", \"Connection\", \"Range\", \"Host\",
--   \"If-Modified-Since\", \"If-Unmodified-Since\" and \"If-Range\".
requestMaxIndex :: Int
requestMaxIndex = fromEnum (maxBound :: RequestHeaderIndex)

requestKeyIndex :: HeaderName -> Int
requestKeyIndex "content-length"      = fromEnum ReqContentLength
requestKeyIndex "transfer-encoding"   = fromEnum ReqTransferEncoding
requestKeyIndex "expect"              = fromEnum ReqExpect
requestKeyIndex "connection"          = fromEnum ReqConnection
requestKeyIndex "range"               = fromEnum ReqRange
requestKeyIndex "host"                = fromEnum ReqHost
requestKeyIndex "if-modified-since"   = fromEnum ReqIfModifiedSince
requestKeyIndex "if-unmodified-since" = fromEnum ReqIfUnmodifiedSince
requestKeyIndex "if-range"            = fromEnum ReqIfRange
requestKeyIndex "referer"             = fromEnum ReqReferer
requestKeyIndex "user-agent"          = fromEnum ReqUserAgent
requestKeyIndex _                     = -1

defaultIndexRequestHeader :: IndexedHeader
defaultIndexRequestHeader = array (0,requestMaxIndex) [(i,Nothing)|i<-[0..requestMaxIndex]]

----------------------------------------------------------------

indexResponseHeader :: ResponseHeaders -> IndexedHeader
indexResponseHeader hdr = traverseHeader hdr responseMaxIndex responseKeyIndex

data ResponseHeaderIndex = ResContentLength
                         | ResServer
                         | ResDate
                         deriving (Enum,Bounded)

-- | The size for 'IndexedHeader' for HTTP Response.
responseMaxIndex :: Int
responseMaxIndex = fromEnum (maxBound :: ResponseHeaderIndex)

responseKeyIndex :: HeaderName -> Int
responseKeyIndex "content-length" = fromEnum ResContentLength
responseKeyIndex "server"         = fromEnum ResServer
responseKeyIndex "date"           = fromEnum ResDate
responseKeyIndex _                = -1

----------------------------------------------------------------

traverseHeader :: [Header] -> Int -> (HeaderName -> Int) -> IndexedHeader
traverseHeader hdr maxidx getIndex = runSTArray $ do
    arr <- newArray (0,maxidx) Nothing
    mapM_ (insert arr) hdr
    return arr
  where
    insert arr (key,val)
      | idx == -1 = return ()
      | otherwise = writeArray arr idx (Just val)
      where
        idx = getIndex key
