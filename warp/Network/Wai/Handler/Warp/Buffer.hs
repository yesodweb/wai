module Network.Wai.Handler.Warp.Buffer (
    createWriteBuffer,
    allocateBuffer,
    freeBuffer,
    toBuilderBuffer,
    bufferIO,
    rawBufferIO,
) where

import Data.IORef (IORef, readIORef)
import qualified Data.Streaming.ByteString.Builder.Buffer as B (Buffer (..))
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (free, mallocBytes)
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
    fptr <- newForeignPtr_ bytes
    return
        WriteBuffer
            { bufBuffer = bytes
            , bufFPtr = fptr
            , bufSize = size
            , bufFree = freeBuffer bytes
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
    let fptr = bufFPtr writeBuffer
        ptr = bufBuffer writeBuffer
        size = bufSize writeBuffer
    return $ B.Buffer fptr ptr ptr (ptr `plusPtr` size)

-- | Slice the given number of bytes out of a 'WriteBuffer' using its
-- cached 'ForeignPtr', without allocating a fresh wrapper.
bufferIO :: WriteBuffer -> Int -> (ByteString -> IO ()) -> IO ()
bufferIO writeBuffer siz io = io $ PS (bufFPtr writeBuffer) 0 siz

-- | Like 'bufferIO' for callers that only have a raw pointer.
-- This allocates a fresh 'ForeignPtr' wrapper on every call.
rawBufferIO :: Buffer -> Int -> (ByteString -> IO ()) -> IO ()
rawBufferIO ptr siz io = do
    fptr <- newForeignPtr_ ptr
    io $ PS fptr 0 siz
