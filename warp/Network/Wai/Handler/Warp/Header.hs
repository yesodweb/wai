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

idxContentLength,idxTransferEncoding,idxExpect :: Int
idxConnection,idxRange,idxHost :: Int
idxIfModifiedSince,idxIfUnmodifiedSince,idxIfRange :: Int
idxContentLength     = 0
idxTransferEncoding  = 1
idxExpect            = 2
idxConnection        = 3
idxRange             = 4
idxHost              = 5
idxIfModifiedSince   = 6
idxIfUnmodifiedSince = 7
idxIfRange           = 8

-- | The size for 'IndexedHeader' for HTTP Request.
--   From 0 to this corresponds to \"Content-Length\", \"Transfer-Encoding\",
--   \"Expect\", \"Connection\", \"Range\", \"Host\",
--   \"If-Modified-Since\", \"If-Unmodified-Since\" and \"If-Range\".
requestMaxIndex :: Int
requestMaxIndex     = 8

requestKeyIndex :: HeaderName -> Int
requestKeyIndex "content-length"      = idxContentLength
requestKeyIndex "transfer-encoding"   = idxTransferEncoding
requestKeyIndex "expect"              = idxExpect
requestKeyIndex "connection"          = idxConnection
requestKeyIndex "range"               = idxRange
requestKeyIndex "host"                = idxHost
requestKeyIndex "if-modified-since"   = idxIfModifiedSince
requestKeyIndex "if-unmodified-since" = idxIfUnmodifiedSince
requestKeyIndex "if-range"            = idxIfRange
requestKeyIndex _                     = -1

defaultIndexRequestHeader :: IndexedHeader
defaultIndexRequestHeader = array (0,requestMaxIndex) [(i,Nothing)|i<-[0..requestMaxIndex]]

----------------------------------------------------------------

indexResponseHeader :: ResponseHeaders -> IndexedHeader
indexResponseHeader hdr = traverseHeader hdr responseMaxIndex responseKeyIndex

idxServer, idxDate :: Int
--idxContentLength = 0
idxServer        = 1
idxDate          = 2

-- | The size for 'IndexedHeader' for HTTP Response.
responseMaxIndex :: Int
responseMaxIndex = 2

responseKeyIndex :: HeaderName -> Int
responseKeyIndex "content-length" = idxContentLength
responseKeyIndex "server"         = idxServer
responseKeyIndex "date"           = idxDate
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
