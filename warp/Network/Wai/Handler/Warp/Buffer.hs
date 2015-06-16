{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.Buffer where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.ByteString.Internal
import Data.ByteString.Unsafe (unsafeTake, unsafeDrop)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Streaming.ByteString.Builder.Buffer as B (Buffer (..))
import Data.Word (Word8)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (mallocBytes, free, finalizerFree)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Network.Wai.Handler.Warp.Types

largeBufferSize :: Int
largeBufferSize = 16384

minBufferSize :: Int
minBufferSize = 2048

newBufferPool :: IO BufferPool
newBufferPool = newIORef BS.empty

mallocBuffer :: Int -> IO ByteString
mallocBuffer size = do
    ptr <- mallocBytes size
    fptr <- newForeignPtr finalizerFree ptr
    return $! PS fptr 0 size
{-# INLINE mallocBuffer #-}

createBuffer :: Int -> IO ByteString
createBuffer size = do
    fptr <- mallocByteString size
    return $! PS fptr 0 size
{-# INLINE createBuffer #-}

usefulBuffer :: ByteString -> Bool
usefulBuffer buffer = BS.length buffer >= minBufferSize
{-# INLINE usefulBuffer #-}

getBuffer :: BufferPool -> IO ByteString
getBuffer pool = do
    buffer <- readIORef pool
    if usefulBuffer buffer then return buffer else mallocBuffer largeBufferSize
{-# INLINE getBuffer #-}

putBuffer :: BufferPool -> ByteString -> IO ()
putBuffer pool buffer = when (usefulBuffer buffer) $ writeIORef pool buffer
{-# INLINE putBuffer #-}

withForeignBuffer :: ByteString -> ((Buffer, BufSize) -> IO Int) -> IO Int
withForeignBuffer (PS ps s l) f = withForeignPtr ps $ \p -> f (castPtr p `plusPtr` s, l)
{-# INLINE withForeignBuffer #-}

withBufferPool :: BufferPool -> ((Buffer, BufSize) -> IO Int) -> IO ByteString
withBufferPool pool f = do
    buffer <- getBuffer pool
    consumed <- withForeignBuffer buffer f
    putBuffer pool $! unsafeDrop consumed buffer
    return $! unsafeTake consumed buffer
{-# INLINE withBufferPool #-}

-- 2^14 = 1024 * 16
-- The maximum size of TLS record
-- The maximum size of HTTP/2 frame payload (excluding frame header)
bufferSize :: BufSize
bufferSize = 16384

allocateBuffer :: Int -> IO Buffer
allocateBuffer = mallocBytes

freeBuffer :: Buffer -> IO ()
freeBuffer = free

toBlazeBuffer :: Buffer -> BufSize -> IO B.Buffer
toBlazeBuffer ptr size = do
    fptr <- newForeignPtr_ ptr
    return $ B.Buffer fptr ptr ptr (ptr `plusPtr` size)

{-# INLINE copy #-}
copy :: Ptr Word8 -> ByteString -> IO (Ptr Word8)
copy !ptr (PS fp o l) = withForeignPtr fp $ \p -> do
    memcpy ptr (p `plusPtr` o) (fromIntegral l)
    return $! ptr `plusPtr` l

{-# INLINE toBS #-}
toBS :: Buffer -> Int -> IO ByteString
toBS ptr siz = do
    fptr <- newForeignPtr_ ptr
    return $ PS fptr 0 siz
