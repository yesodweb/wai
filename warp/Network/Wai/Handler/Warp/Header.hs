{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

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
requestKeyIndex hn = case BS.length bs of
   4  -> if bs == "host" then fromEnum ReqHost else -1
   5  -> if bs == "range" then fromEnum ReqRange else -1
   6  -> if bs == "expect" then fromEnum ReqExpect else -1
   7  -> if bs == "referer" then fromEnum ReqReferer else -1
   8  -> if bs == "if-range" then fromEnum ReqIfRange else -1
   10 -> if bs == "user-agent" then fromEnum ReqUserAgent else
         if bs == "connection" then fromEnum ReqConnection else -1
   14 -> if bs == "content-length" then fromEnum ReqContentLength else -1
   17 -> if bs == "transfer-encoding" then fromEnum ReqTransferEncoding else
         if bs == "if-modified-since" then fromEnum ReqIfModifiedSince
         else -1
   19 -> if bs == "if-unmodified-since" then fromEnum ReqIfUnmodifiedSince else -1
   _  -> -1
  where
    bs = foldedCase hn

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
responseKeyIndex hn = case BS.length bs of
    4  -> if bs == "date" then fromEnum ResDate else -1
    6  -> if bs == "server" then fromEnum ResServer else -1
    14 -> if bs == "content-length" then fromEnum ResContentLength else -1
    _  -> -1
  where
    bs = foldedCase hn

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
