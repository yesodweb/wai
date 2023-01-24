{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Socket.BufferPool.Recv (
    receive
  , makeRecvN
  ) where

import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..), unsafeCreate)
import Data.IORef
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr)
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
    fromIntegral <$> tryReceive fd ptr size'

----------------------------------------------------------------

tryReceive :: CInt -> Buffer -> CSize -> IO CInt
tryReceive sock ptr size = go
  where
    go = do
#ifdef mingw32_HOST_OS
      bytes <- windowsThreadBlockHack $ fromIntegral <$> readRawBufferPtr "tryReceive" (FD sock 1) (castPtr ptr) 0 size
#else
      bytes <- c_recv sock (castPtr ptr) size 0
#endif
      if bytes == -1 then do
          errno <- getErrno
          if errno == eAGAIN then do
              threadWaitRead (Fd sock)
              go
            else
              throwErrno "tryReceive"
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
makeRecvN :: ByteString -> Recv -> IO RecvN
makeRecvN bs0 recv = do
    ref <- newIORef bs0
    return $ recvN ref recv

-- | The receiving function which receives exactly N bytes
--   (the fourth argument).
recvN :: IORef ByteString -> Recv -> RecvN
recvN ref recv size = do
    cached <- readIORef ref
    (bs, leftover) <- tryRecvN cached size recv
    writeIORef ref leftover
    return bs

----------------------------------------------------------------

tryRecvN :: ByteString -> Int -> IO ByteString -> IO (ByteString, ByteString)
tryRecvN init0 siz0 recv
  | siz0 <= len0 = return $ BS.splitAt siz0 init0
  | otherwise    = go (init0:) (siz0 - len0)
  where
    len0 = BS.length init0
    go build left = do
        bs <- recv
        let len = BS.length bs
        if len == 0 then
            return ("", "")
          else if len >= left then do
            let (consume, leftover) = BS.splitAt left bs
                ret = concatN siz0 $ build [consume]
            return (ret, leftover)
          else do
            let build' = build . (bs :)
                left' = left - len
            go build' left'

concatN :: Int -> [ByteString] -> ByteString
concatN total bss0 = unsafeCreate total $ \ptr -> goCopy bss0 ptr
  where
    goCopy []       _   = return ()
    goCopy (bs:bss) ptr = do
        ptr' <- copy ptr bs
        goCopy bss ptr'

#ifndef mingw32_HOST_OS
-- fixme: the type of the return value
foreign import ccall unsafe "recv"
    c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
#endif
