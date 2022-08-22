module Network.Socket.BufferPool.Buffer (
    newBufferPool
  , withBufferPool
  , mallocBS
  , copy
  ) where

import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..), memcpy)
import Data.ByteString.Unsafe (unsafeTake, unsafeDrop)
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (mallocBytes, finalizerFree)
import Foreign.Ptr (castPtr, plusPtr)

import Network.Socket.BufferPool.Types

----------------------------------------------------------------

-- | Creating a buffer pool.
--   The first argument is the lower limit.
--   When the size of the buffer in the poll is lower than this limit,
--   the buffer is thrown awany (and is eventually freed).
--   Then a new buffer is allocated.
--   The second argument is the size for the new allocation.
newBufferPool :: Int -> Int -> IO BufferPool
newBufferPool l h = BufferPool l h <$> newIORef BS.empty

----------------------------------------------------------------

-- | Using a buffer pool.
--   The second argument is a function which returns
--   how many bytes are filled in the buffer.
--   The buffer in the buffer pool is automatically managed.
withBufferPool :: BufferPool -> (Buffer -> BufSize -> IO Int) -> IO ByteString
withBufferPool (BufferPool l h ref) f = do
    buf0 <- readIORef ref
    buf  <- if BS.length buf0 >= l then return buf0
                                   else mallocBS h
    consumed <- withForeignBuffer buf f
    writeIORef ref $ unsafeDrop consumed buf
    return $ unsafeTake consumed buf

withForeignBuffer :: ByteString -> (Buffer -> BufSize -> IO Int) -> IO Int
withForeignBuffer (PS ps s l) f = withForeignPtr ps $ \p -> f (castPtr p `plusPtr` s) l
{-# INLINE withForeignBuffer #-}

----------------------------------------------------------------

-- | Allocating a byte string.
mallocBS :: Int -> IO ByteString
mallocBS size = do
    ptr <- mallocBytes size
    fptr <- newForeignPtr finalizerFree ptr
    return $ PS fptr 0 size
{-# INLINE mallocBS #-}

-- | Copying the bytestring to the buffer.
--   This function returns the point where the next copy should start.
copy :: Buffer -> ByteString -> IO Buffer
copy ptr (PS fp o l) = withForeignPtr fp $ \p -> do
    memcpy ptr (p `plusPtr` o) (fromIntegral l)
    return $ ptr `plusPtr` l
{-# INLINE copy #-}
