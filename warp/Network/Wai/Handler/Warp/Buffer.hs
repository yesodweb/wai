module Network.Wai.Handler.Warp.Buffer where

import qualified Blaze.ByteString.Builder.Internal.Buffer as B (Buffer (..))
import Data.Word (Word8)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr, plusPtr)

type Buffer = Ptr Word8

-- FIXME come up with good values here
bytesPerRead :: Int
bytesPerRead = 4096

bytesPerWrite :: Int
bytesPerWrite = 4096

allocateBuffer :: Int -> IO Buffer
allocateBuffer = mallocBytes

freeBuffer :: Buffer -> IO ()
freeBuffer = free

toBlazeBuffer :: Buffer -> Int -> IO B.Buffer
toBlazeBuffer ptr size = do
    fptr <- newForeignPtr_ ptr
    return $ B.Buffer fptr ptr ptr (ptr `plusPtr` size)
