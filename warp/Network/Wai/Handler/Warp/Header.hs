{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.Header where

import Data.Array
import Data.Array.ST
import Network.HTTP.Types
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

type IndexedHeader = Array Int (Maybe HeaderValue)

----------------------------------------------------------------

indexRequestHeader :: RequestHeaders -> IndexedHeader
indexRequestHeader hdr = traverseHeader hdr requestMaxIndex

idxContentLength,idxTransferEncoding,idxExpect :: Int
idxConnection,idxRange,idxHost :: Int
idxContentLength    = 0
idxTransferEncoding = 1
idxExpect           = 2
idxConnection       = 3
idxRange            = 4
idxHost             = 5

requestMaxIndex :: Int
requestMaxIndex     = 5

----------------------------------------------------------------

traverseHeader :: [Header] -> Int -> IndexedHeader
traverseHeader hdr maxidx = runSTArray $ do
    arr <- newArray (0,maxidx) Nothing
    mapM_ (insert arr) hdr
    return arr
  where
    insert arr (key,val)
      | idx == -1 = return ()
      | otherwise = writeArray arr idx (Just val)
      where
        idx = keyIndex key

keyIndex :: HeaderName -> Int
keyIndex "content-length"    = idxContentLength
keyIndex "transfer-encoding" = idxTransferEncoding
keyIndex "expect"            = idxExpect
keyIndex "connection"        = idxConnection
keyIndex "range"             = idxRange
keyIndex "host"              = idxHost
keyIndex _                   = -1
