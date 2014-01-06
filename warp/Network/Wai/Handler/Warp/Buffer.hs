module Network.Wai.Handler.Warp.Buffer where

import qualified Blaze.ByteString.Builder.Internal.Buffer as B (Buffer (..))
import Data.Word (Word8)
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Marshal.Alloc (mallocBytes, free)
import Foreign.Ptr (Ptr, plusPtr)

type Buffer = Ptr Word8
type BufSize = Int

-- FIXME come up with good values here
bufferSize :: BufSize
bufferSize = 4096

allocateBuffer :: Int -> IO Buffer
allocateBuffer = mallocBytes

freeBuffer :: Buffer -> IO ()
freeBuffer = free

toBlazeBuffer :: Buffer -> BufSize -> IO B.Buffer
toBlazeBuffer ptr size = do
    fptr <- newForeignPtr_ ptr
    return $ B.Buffer fptr ptr ptr (ptr `plusPtr` size)
