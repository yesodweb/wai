{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Socket.BufferPool.Recv (
    receive
  , receiveBuf
  , makeReceiveN
  , makePlainReceiveN
  ) where

import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..))
import Data.IORef
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Conc (threadWaitRead)
import Network.Socket (Socket, withFdSocket)
import System.Posix.Types (Fd(..))

#ifdef mingw32_HOST_OS
import GHC.IO.FD (FD(..), readRawBufferPtr)
import Network.Socket.BufferPool.Windows
#endif

import Network.Socket.BufferPool.Types
import Network.Socket.BufferPool.Buffer

----------------------------------------------------------------

-- | The receiving function with a buffer pool.
--   The buffer pool is automatically managed.
receive :: Socket -> BufferPool -> Recv
receive sock pool = withBufferPool pool $ \ptr size -> do
#if MIN_VERSION_network(3,1,0)
  withFdSocket sock $ \fd -> do
#elif MIN_VERSION_network(3,0,0)
    fd <- fdSocket sock
#else
    let fd = fdSocket sock
#endif
    let size' = fromIntegral size
    fromIntegral <$> tryRecv fd ptr size'

----------------------------------------------------------------

-- | The receiving function with a buffer.
--   This tries to fill the buffer.
--   This returns when the buffer is filled or reaches EOF.
receiveBuf :: Socket -> RecvBuf
receiveBuf sock buf0 siz0 = do
#if MIN_VERSION_network(3,1,0)
  withFdSocket sock $ \fd -> do
#elif MIN_VERSION_network(3,0,0)
    fd <- fdSocket sock
#else
    let fd = fdSocket sock
#endif
    loop fd buf0 siz0
  where
    loop _  _   0   = return True
    loop fd buf siz = do
        n <- fromIntegral <$> tryRecv fd buf (fromIntegral siz)
        -- fixme: what should we do in the case of n == 0
        if n == 0 then
            return False
          else
            loop fd (buf `plusPtr` n) (siz - n)

----------------------------------------------------------------

tryRecv :: CInt -> Buffer -> CSize -> IO CInt
tryRecv sock ptr size = go
  where
    go = do
#ifdef mingw32_HOST_OS
      bytes <- windowsThreadBlockHack $ fromIntegral <$> readRawBufferPtr "tryRecv" (FD sock 1) (castPtr ptr) 0 size
#else
      bytes <- c_recv sock (castPtr ptr) size 0
#endif
      if bytes == -1 then do
          errno <- getErrno
          if errno == eAGAIN then do
              threadWaitRead (Fd sock)
              go
            else
              throwErrno "tryRecv"
         else
          return bytes

----------------------------------------------------------------

-- | This function returns a receiving function
--   based on two receiving functions.
--   The returned function receives exactly N bytes.
--   The first argument is an initial received data.
--   After consuming the initial data, the two functions is used.
--   When N is less than equal to 4096, the buffer pool is used.
--   Otherwise, a new buffer is allocated.
--   In this case, the global lock is taken.
makeReceiveN :: ByteString -> Recv -> RecvBuf -> IO RecvN
makeReceiveN bs0 recv recvBuf = do
    ref <- newIORef bs0
    return $ receiveN ref recv recvBuf

-- | This function returns a receiving function with two receiving
--   functions is created internally.
--   The second argument is the lower limit of the buffer pool.
--   The third argument is the size of the allocated buffer in the pool.
--   The fourth argument is an initial received data.
--   The returned function behaves as described in 'makeReceiveN'.
makePlainReceiveN :: Socket -> Int -> Int -> ByteString -> IO RecvN
makePlainReceiveN s l h bs0 = do
    ref <- newIORef bs0
    pool <- newBufferPool l h
    return $ receiveN ref (receive s pool) (receiveBuf s)

-- | The receiving function which receives exactly N bytes
--   (the fourth argument).
receiveN :: IORef ByteString -> Recv -> RecvBuf -> RecvN
receiveN ref recv recvBuf size = do
    cached <- readIORef ref
    (bs, leftover) <- tryRecvN cached size recv recvBuf
    writeIORef ref leftover
    return bs

----------------------------------------------------------------

tryRecvN :: ByteString -> Int -> IO ByteString -> RecvBuf -> IO (ByteString, ByteString)
tryRecvN init0 siz0 recv recvBuf
  | siz0 <= len0 = return $ BS.splitAt siz0 init0
  -- fixme: hard coding 4096
  | siz0 <= 4096 = recvWithPool [init0] (siz0 - len0)
  | otherwise    = recvWithNewBuf
  where
    len0 = BS.length init0
    recvWithPool bss siz = do
        bs <- recv
        let len = BS.length bs
        if len == 0 then
            return ("", "")
          else if len >= siz then do
            let (consume, leftover) = BS.splitAt siz bs
                ret = BS.concat $ reverse (consume : bss)
            return (ret, leftover)
          else do
            let bss' = bs : bss
                siz' = siz - len
            recvWithPool bss' siz'
    recvWithNewBuf = do
      bs@(PS fptr _ _) <- mallocBS siz0
      withForeignPtr fptr $ \ptr -> do
          ptr' <- copy ptr init0
          full <- recvBuf ptr' (siz0 - len0)
          if full then
              return (bs, "")
            else
              return ("", "") -- fixme

#ifndef mingw32_HOST_OS
-- fixme: the type of the return value
foreign import ccall unsafe "recv"
    c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
#endif
