{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.ResponseHeader (composeHeader) where

import qualified Data.ByteString as S
import Data.ByteString.Internal (create)
import qualified Data.CaseInsensitive as CI
import Foreign.Ptr
import GHC.Storable
import qualified Network.HTTP.Types as H

import Network.Wai.Handler.Warp.Buffer (copy)
import Network.Wai.Handler.Warp.Imports

----------------------------------------------------------------

composeHeader :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> IO ByteString
composeHeader !httpversion !status !responseHeaders = create len $ \ptr -> do
    ptr1 <- copyStatus ptr httpversion status
    ptr2 <- copyHeaders ptr1 responseHeaders
    void $ copyCRLF ptr2
  where
    !len = 17 + slen + foldl' fieldLength 0 responseHeaders
    fieldLength !l (!k,!v) = l + S.length (CI.original k) + S.length v + 4
    !slen = S.length $ H.statusMessage status

httpVer11 :: ByteString
httpVer11 = "HTTP/1.1 "

httpVer10 :: ByteString
httpVer10 = "HTTP/1.0 "

{-# INLINE copyStatus #-}
copyStatus :: Ptr Word8 -> H.HttpVersion -> H.Status -> IO (Ptr Word8)
copyStatus !ptr !httpversion !status = do
    ptr1 <- copy ptr httpVer
    writeWord8OffPtr ptr1 0 (zero + fromIntegral r2)
    writeWord8OffPtr ptr1 1 (zero + fromIntegral r1)
    writeWord8OffPtr ptr1 2 (zero + fromIntegral r0)
    writeWord8OffPtr ptr1 3 spc
    ptr2 <- copy (ptr1 `plusPtr` 4) (H.statusMessage status)
    copyCRLF ptr2
  where
    httpVer
      | httpversion == H.HttpVersion 1 1 = httpVer11
      | otherwise = httpVer10
    (q0,r0) = H.statusCode status `divMod` 10
    (q1,r1) = q0 `divMod` 10
    r2 = q1 `mod` 10

{-# INLINE copyHeaders #-}
copyHeaders :: Ptr Word8 -> [H.Header] -> IO (Ptr Word8)
copyHeaders !ptr [] = return ptr
copyHeaders !ptr (h:hs) = do
    ptr1 <- copyHeader ptr h
    copyHeaders ptr1 hs

{-# INLINE copyHeader #-}
copyHeader :: Ptr Word8 -> H.Header -> IO (Ptr Word8)
copyHeader !ptr (k,v) = do
    ptr1 <- copy ptr (CI.original k)
    writeWord8OffPtr ptr1 0 colon
    writeWord8OffPtr ptr1 1 spc
    ptr2 <- copy (ptr1 `plusPtr` 2) v
    copyCRLF ptr2

{-# INLINE copyCRLF #-}
copyCRLF :: Ptr Word8 -> IO (Ptr Word8)
copyCRLF !ptr = do
    writeWord8OffPtr ptr 0 cr
    writeWord8OffPtr ptr 1 lf
    return $! ptr `plusPtr` 2

zero :: Word8
zero = 48
spc :: Word8
spc = 32
colon :: Word8
colon = 58
cr :: Word8
cr = 13
lf :: Word8
lf = 10
