{-# LANGUAGE ForeignFunctionInterface, OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Recv (
    receive
  , receiveBuf
  , makeReceiveN
  , makePlainReceiveN
  , spell
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..))
import Data.Word (Word8)
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import GHC.Conc (threadWaitRead)
import Network.Socket (Socket, fdSocket)
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.IORef
import Network.Wai.Handler.Warp.Types
import System.Posix.Types (Fd(..))

#ifdef mingw32_HOST_OS
import GHC.IO.FD (FD(..), readRawBufferPtr)
import Network.Wai.Handler.Warp.Windows
#endif

----------------------------------------------------------------

makeReceiveN :: ByteString -> Recv -> RecvBuf -> IO (BufSize -> IO ByteString)
makeReceiveN bs0 recv recvBuf = do
    ref <- newIORef bs0
    return $ receiveN ref recv recvBuf

makePlainReceiveN :: Socket -> ByteString -> IO (BufSize -> IO ByteString)
makePlainReceiveN s bs0 = do
    ref <- newIORef bs0
    pool <- newBufferPool
    return $ receiveN ref (receive s pool) (receiveBuf s)

receiveN :: IORef ByteString -> Recv -> RecvBuf -> BufSize -> IO ByteString
receiveN ref recv recvBuf size = do
    cached <- readIORef ref
    (bs, leftover) <- spell cached size recv recvBuf
    writeIORef ref leftover
    return bs

----------------------------------------------------------------

spell :: ByteString -> BufSize -> IO ByteString -> RecvBuf -> IO (ByteString, ByteString)
spell initial siz recv recvBuf
  | siz <= len0 = return $ BS.splitAt siz initial
  | siz <= 4096 = do
      bs <- recv
      if bs == "" then
          return ("", "")
        else do
          let (bs1, leftover) = BS.splitAt (siz - len0) bs
          return (BS.append initial bs1, leftover)
  | otherwise = do
      bs@(PS fptr _ _) <- mallocByteString siz
      withForeignPtr fptr $ \ptr -> do
          ptr' <- copy ptr initial
          full <- recvBuf ptr' (siz - len0)
          if full then
              return (bs, "")
             else
              return ("", "") -- fixme
  where
    len0 = BS.length initial

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
    bytes <- windowsThreadBlockHack $ fmap fromIntegral $ readRawBufferPtr "recv" (FD sock 1) (castPtr ptr) 0 size
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
