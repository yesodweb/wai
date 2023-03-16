module Network.Wai.Handler.Warp.Buffer (
    createWriteBuffer
  , allocateBuffer
  , freeBuffer
  , toBuilderBuffer
  , bufferIO
  ) where

import Data.IORef (IORef, readIORef)
import qualified Data.Streaming.ByteString.Builder.Buffer as B (Buffer (..))
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (plusPtr)
import Network.Socket.BufferPool

import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | Allocate a buffer of the given size and wrap it in a 'WriteBuffer'
-- containing that size and a finalizer.
createWriteBuffer :: BufSize -> IO WriteBuffer
createWriteBuffer size = do
  bytes <- allocateBuffer size
  return
    WriteBuffer
      { bufBuffer = bytes,
        bufSize = size,
        bufFree = freeBuffer bytes
      }

----------------------------------------------------------------

-- | Allocating a buffer with malloc().
allocateBuffer :: Int -> IO Buffer
allocateBuffer = mallocBytes

-- | Releasing a buffer with free().
freeBuffer :: Buffer -> IO ()
freeBuffer = free

----------------------------------------------------------------
--
-- Utilities
--

toBuilderBuffer :: IORef WriteBuffer -> IO B.Buffer
toBuilderBuffer writeBufferRef = do
    writeBuffer <- readIORef writeBufferRef
    let ptr = bufBuffer writeBuffer
        size = bufSize writeBuffer
    fptr <- newForeignPtr_ ptr
    return $ B.Buffer fptr ptr ptr (ptr `plusPtr` size)

bufferIO :: Buffer -> Int -> (ByteString -> IO ()) -> IO ()
bufferIO ptr siz io = do
    fptr <- newForeignPtr_ ptr
    io $ PS fptr 0 siz
