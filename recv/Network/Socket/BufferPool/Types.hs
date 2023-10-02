module Network.Socket.BufferPool.Types where

import Data.ByteString (ByteString)
import Data.IORef
import Data.Word (Word8)
import Foreign.Ptr (Ptr)

-- | Type for buffer.
type Buffer = Ptr Word8

-- | Type for buffer size.
type BufSize = Int

-- | Type for read buffer pool.
data BufferPool = BufferPool {
    minBufSize :: Int -- ^ If the buffer is larger than or equal to this size,
                      --   the buffer is used.
                      --   Otherwise, a new buffer is allocated.
                      --   The thrown buffer is eventually freed.
  , maxBufSize :: Int
  , poolBuffer :: IORef ByteString
  }

-- | Type for the receiving function with a buffer pool.
type Recv = IO ByteString

-- | Type for the receiving function which receives N bytes.
type RecvN = Int -> IO ByteString
