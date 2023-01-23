-- | This module provides efficient receiving functions from the network.
--   'Network.Socket.ByteString.recv' uses 'createAndTrim'
--   which behaves as follows:
--
--   * Allocates a buffer whose size is decided from the
--     first argument.
--   * Receives data with the buffer.
--   * Allocates another buffer whose size fits the received data.
--   * Copies the data from the first buffer to the second buffer.
--
--  On 64bit machines, the global lock is taken for the allocation of
--  a byte string whose length is larger than or equal to 3272 bytes.
--  So, for instance, if 4,096 is specified to 'recv' and the size of
--  received data is 3,300, the global lock is taken twice with the copy
--  overhead.
--
--  The efficient receiving functions provided here use a buffer pool.
--  A large buffer is allocated at the beginning and it is divided into
--  a used one and a leftover when receiving.
--  The latter is kept in the buffer pool and will be used next time.
--  When the buffer gets small
--  and usefless, a new large buffer is allocated.
module Network.Socket.BufferPool (
  -- * Recv
    Recv
  , receive
  , BufferPool
  , newBufferPool
  , withBufferPool
  -- * RecvN
  , RecvN
  , makeRecvN
  -- * Types
  , Buffer
  , BufSize
  -- * Utilities
  , mallocBS
  , copy
  ) where

import Network.Socket.BufferPool.Buffer
import Network.Socket.BufferPool.Recv
import Network.Socket.BufferPool.Types
