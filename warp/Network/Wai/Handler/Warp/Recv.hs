{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Recv (
    receive
  , receiveBuf
  , makeReceiveN
  , makePlainReceiveN
  , spell
  ) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.IORef
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Conc (threadWaitRead)
import Network.Socket (Socket, fdSocket)
import System.Posix.Types (Fd(..))

import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

#ifdef mingw32_HOST_OS
import GHC.IO.FD (FD(..), readRawBufferPtr)
import Network.Wai.Handler.Warp.Windows
#endif

----------------------------------------------------------------

makeReceiveN :: ByteString -> Recv -> RecvBuf -> IO (BufSize -> IO ByteString)
makeReceiveN bs0 recv recvBuf = do
    ref <- newIORef bs0
    return $ receiveN ref recv recvBuf

-- | This function returns a receiving function
--   based on two receiving functions.
--   The returned function efficiently manages received data
--   which is initialized by the first argument.
--   The returned function may allocate a byte string with malloc().
makePlainReceiveN :: Socket -> ByteString -> IO (BufSize -> IO ByteString)
makePlainReceiveN s bs0 = do
    ref <- newIORef bs0
    pool <- newBufferPool
    return $ receiveN ref (receive s pool) (receiveBuf s)

receiveN :: IORef ByteString -> Recv -> RecvBuf -> BufSize -> IO ByteString
receiveN ref recv recvBuf size = E.handle handler $ do
    cached <- readIORef ref
    (bs, leftover) <- spell cached size recv recvBuf
    writeIORef ref leftover
    return bs
 where
   handler :: E.SomeException -> IO ByteString
   handler _ = return ""

----------------------------------------------------------------

spell :: ByteString -> BufSize -> IO ByteString -> RecvBuf -> IO (ByteString, ByteString)
spell init0 siz0 recv recvBuf
  | siz0 <= len0 = return $ BS.splitAt siz0 init0
  -- fixme: hard coding 4096
  | siz0 <= 4096 = loop [init0] (siz0 - len0)
  | otherwise    = do
      bs@(PS fptr _ _) <- mallocBS siz0
      withForeignPtr fptr $ \ptr -> do
          ptr' <- copy ptr init0
          full <- recvBuf ptr' (siz0 - len0)
          if full then
              return (bs, "")
            else
              return ("", "") -- fixme
  where
    len0 = BS.length init0
    loop bss siz = do
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
            loop bss' siz'

receive :: Socket -> BufferPool -> Recv
receive sock pool = withBufferPool pool $ \ (ptr, size) -> do
    let sock' = fdSocket sock
        size' = fromIntegral size
    fromIntegral <$> receiveloop sock' ptr size'

receiveBuf :: Socket -> RecvBuf
receiveBuf sock buf0 siz0 = loop buf0 siz0
  where
    loop _   0   = return True
    loop buf siz = do
        n <- fromIntegral <$> receiveloop fd buf (fromIntegral siz)
        -- fixme: what should we do in the case of n == 0
        if n == 0 then
            return False
          else
            loop (buf `plusPtr` n) (siz - n)
    fd = fdSocket sock

receiveloop :: CInt -> Ptr Word8 -> CSize -> IO CInt
receiveloop sock ptr size = do
#ifdef mingw32_HOST_OS
    bytes <- windowsThreadBlockHack $ fromIntegral <$> readRawBufferPtr "recv" (FD sock 1) (castPtr ptr) 0 size
#else
    bytes <- c_recv sock (castPtr ptr) size 0
#endif
    if bytes == -1 then do
        errno <- getErrno
        if errno == eAGAIN then do
            threadWaitRead (Fd sock)
            receiveloop sock ptr size
          else
            throwErrno "receiveloop"
       else
        return bytes

-- fixme: the type of the return value
foreign import ccall unsafe "recv"
    c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt
