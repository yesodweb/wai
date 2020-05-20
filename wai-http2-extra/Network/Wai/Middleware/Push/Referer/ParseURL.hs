{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Push.Referer.ParseURL (
    parseUrl
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Internal (ByteString(..), memchr)
import Data.Word8
import Foreign.ForeignPtr (withForeignPtr, ForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, minusPtr, nullPtr)
import Foreign.Storable (peek)

import Network.Wai.Middleware.Push.Referer.Types

-- |
--
-- >>> parseUrl ""
-- (Nothing,"")
-- >>> parseUrl "/"
-- (Nothing,"/")
-- >>> parseUrl "ht"
-- (Nothing,"")
-- >>> parseUrl "http://example.com/foo/bar/"
-- (Just "example.com","/foo/bar/")
-- >>> parseUrl "https://www.example.com/path/to/dir/"
-- (Just "www.example.com","/path/to/dir/")
-- >>> parseUrl "http://www.example.com:8080/path/to/dir/"
-- (Just "www.example.com:8080","/path/to/dir/")
-- >>> parseUrl "//www.example.com:8080/path/to/dir/"
-- (Just "www.example.com:8080","/path/to/dir/")
-- >>> parseUrl "/path/to/dir/"
-- (Nothing,"/path/to/dir/")

parseUrl :: ByteString -> IO (Maybe ByteString, URLPath)
parseUrl bs@(PS fptr0 off len)
  | len == 0 = return (Nothing, "")
  | len == 1 = return (Nothing, bs)
  | otherwise = withForeignPtr fptr0 $ \ptr0 -> do
      let begptr = ptr0 `plusPtr` off
          limptr = begptr `plusPtr` len
      parseUrl' fptr0 ptr0 begptr limptr len

parseUrl' :: ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int
          -> IO (Maybe ByteString, URLPath)
parseUrl' fptr0 ptr0 begptr limptr len0 = do
      w0 <- peek begptr
      if w0 == _slash then do
          w1 <- peek $ begptr `plusPtr` 1
          if w1 == _slash  then
              doubleSlashed begptr len0
            else
              slashed begptr len0 Nothing
        else do
          colonptr <- memchr begptr _colon $ fromIntegral len0
          if colonptr == nullPtr then
              return (Nothing, "")
            else do
              let authptr = colonptr `plusPtr` 1
              doubleSlashed authptr (limptr `minusPtr` authptr)
  where
    -- // / ?
    doubleSlashed :: Ptr Word8 -> Int -> IO (Maybe ByteString, URLPath)
    doubleSlashed ptr len
      | len < 2  = return (Nothing, "")
      | otherwise = do
          let ptr1 = ptr `plusPtr` 2
          pathptr <- memchr ptr1 _slash $ fromIntegral len
          if pathptr == nullPtr then
              return (Nothing, "")
            else do
              let auth = bs ptr0 ptr1 pathptr
              slashed pathptr (limptr `minusPtr` pathptr) (Just auth)

    -- / ?
    slashed :: Ptr Word8 -> Int -> Maybe ByteString -> IO (Maybe ByteString, URLPath)
    slashed ptr len mauth = do
        questionptr <- memchr ptr _question $ fromIntegral len
        if questionptr == nullPtr then do
            let path = bs ptr0 ptr limptr
            return (mauth, path)
          else do
            let path = bs ptr0 ptr questionptr
            return (mauth, path)
    bs p0 p1 p2 = path
      where
        off = p1 `minusPtr` p0
        siz = p2 `minusPtr` p1
        path = PS fptr0 off siz
