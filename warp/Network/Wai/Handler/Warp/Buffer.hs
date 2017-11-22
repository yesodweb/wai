{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.Buffer (
    bufferSize
  , allocateBuffer
  , freeBuffer
  , mallocBS
  , newBufferPool
  , withBufferPool
  , toBuilderBuffer
  , copy
  , bufferIO
  ) where

import qualified Data.ByteString as BS
import Data.ByteString.Internal (memcpy)
import Data.ByteString.Unsafe (unsafeTake, unsafeDrop)
import Data.IORef (newIORef, readIORef, writeIORef)
import qualified Data.Streaming.ByteString.Builder.Buffer as B (Buffer (..))
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (mallocBytes, free, finalizerFree)
import Foreign.Ptr (castPtr, plusPtr)

import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | The default size of the write buffer: 16384 (2^14 = 1024 * 16).
--   This is the maximum size of TLS record.
--   This is also the maximum size of HTTP/2 frame payload
--   (excluding frame header).
bufferSize :: BufSize
bufferSize = 16384

-- | Allocating a buffer with malloc().
allocateBuffer :: Int -> IO Buffer
allocateBuffer = mallocBytes

-- | Releasing a buffer with free().
freeBuffer :: Buffer -> IO ()
freeBuffer = free

----------------------------------------------------------------

largeBufferSize :: Int
largeBufferSize = 16384

minBufferSize :: Int
minBufferSize = 2048

newBufferPool :: IO BufferPool
newBufferPool = newIORef BS.empty

mallocBS :: Int -> IO ByteString
mallocBS size = do
    ptr <- allocateBuffer size
    fptr <- newForeignPtr finalizerFree ptr
    return $! PS fptr 0 size
{-# INLINE mallocBS #-}

usefulBuffer :: ByteString -> Bool
usefulBuffer buffer = BS.length buffer >= minBufferSize
{-# INLINE usefulBuffer #-}

getBuffer :: BufferPool -> IO ByteString
getBuffer pool = do
    buffer <- readIORef pool
    if usefulBuffer buffer then return buffer else mallocBS largeBufferSize
{-# INLINE getBuffer #-}

putBuffer :: BufferPool -> ByteString -> IO ()
putBuffer pool buffer = writeIORef pool buffer
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

----------------------------------------------------------------
--
-- Utilities
--

toBuilderBuffer :: Buffer -> BufSize -> IO B.Buffer
toBuilderBuffer ptr size = do
    fptr <- newForeignPtr_ ptr
    return $ B.Buffer fptr ptr ptr (ptr `plusPtr` size)

-- | Copying the bytestring to the buffer.
--   This function returns the point where the next copy should start.
copy :: Buffer -> ByteString -> IO Buffer
copy !ptr (PS fp o l) = withForeignPtr fp $ \p -> do
    memcpy ptr (p `plusPtr` o) (fromIntegral l)
    return $! ptr `plusPtr` l
{-# INLINE copy #-}

bufferIO :: Buffer -> Int -> (ByteString -> IO ()) -> IO ()
bufferIO ptr siz io = do
    fptr <- newForeignPtr_ ptr
    io $ PS fptr 0 siz
