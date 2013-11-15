{-# LANGUAGE ForeignFunctionInterface #-}

-- fixme: windows support

module Network.Wai.Handler.Warp.Recv (
    ReceiveBuffer
  , receive
  , allocateRecvBuffer
  , freeRecvBuffer
  , bytesPerRead
  ) where

import Control.Applicative ((<$>))
import Control.Monad (void)
import qualified Data.ByteString as BS (empty)
import Data.ByteString.Internal (ByteString(..), mallocByteString)
import Data.Word (Word8)
import Foreign.C.Error (eAGAIN, getErrno, throwErrno)
import Foreign.C.Types
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)
import GHC.Conc (threadWaitRead)
import Network.Socket (Socket, fdSocket)
import System.Posix.Types (Fd(..))
import Foreign.Marshal.Alloc (mallocBytes, free)

----------------------------------------------------------------

type ReceiveBuffer = Ptr Word8

allocateRecvBuffer :: Int -> IO ReceiveBuffer
allocateRecvBuffer = mallocBytes

freeRecvBuffer :: ReceiveBuffer -> IO ()
freeRecvBuffer = free

receive :: Socket -> ReceiveBuffer -> Int -> IO ByteString
receive sock buf size = do
    bytes <- fromIntegral <$> receiveloop sock' buf' size'
    if bytes == 0 then
        return BS.empty
      else do
        fptr <- mallocByteString bytes
        void $ withForeignPtr fptr $ \ptr ->
            c_memcpy ptr buf (fromIntegral bytes)
        return $! PS fptr 0 bytes
  where
    sock' = fdSocket sock
    buf' = castPtr buf
    size' = fromIntegral size

receiveloop :: CInt -> Ptr CChar -> CSize -> IO CInt
receiveloop sock buf size = do
    bytes <- c_recv sock buf size 0
    if bytes == -1 then do
        errno <- getErrno
        if errno == eAGAIN then do
            threadWaitRead (Fd sock)
            receiveloop sock buf size
          else
            throwErrno "receiveloop"
       else
        return bytes

-- FIXME come up with good values here
bytesPerRead :: Int
bytesPerRead = 4096

-- fixme: the type of the return value
foreign import ccall unsafe "recv"
    c_recv :: CInt -> Ptr CChar -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "string.h memcpy"
    c_memcpy :: Ptr Word8 -> Ptr Word8 -> CSize -> IO (Ptr Word8)
