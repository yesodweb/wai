module Network.Socket.BufferPool.Buffer (
    newBufferPool,
    withBufferPool,
    tryWithBufferPool,
    mallocBS,
    copy,
) where

import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString (..))
import Data.IORef (newIORef, readIORef, writeIORef)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc (finalizerFree, mallocBytes)
import Foreign.Marshal.Utils (copyBytes)
import Foreign.Ptr (castPtr, plusPtr)

import Network.Socket.BufferPool.Types

----------------------------------------------------------------

-- | Creating a buffer pool.
--   The first argument is the lower limit.
--   When the size of the buffer in the poll is lower than this limit,
--   the buffer is thrown awany (and is eventually freed).
--   Then a new buffer is allocated.
--   The second argument is the size for the new allocation.
newBufferPool :: Int -> Int -> IO BufferPool
newBufferPool l h = BufferPool l h <$> newIORef BS.empty

----------------------------------------------------------------

-- | Using a buffer pool.
--   The second argument is a function which returns
--   how many bytes are filled in the buffer.
--   This function should return non negative 'Int'.
--   The buffer in the buffer pool is automatically managed.
withBufferPool :: BufferPool -> (Buffer -> BufSize -> IO Int) -> IO ByteString
withBufferPool pool@(BufferPool _ _ ref) f = do
    (buf, consumed) <- applyBufferPool pool f
    writeIORef ref $ BS.drop consumed buf
    return $ BS.take consumed buf

-- | L ike 'withBufferPool' for fillers that can decline to fill:
--   a negative return value from the filler leaves the pool untouched
--   and produces 'Nothing'.
tryWithBufferPool
    :: BufferPool -> (Buffer -> BufSize -> IO Int) -> IO (Maybe ByteString)
tryWithBufferPool pool@(BufferPool _ _ ref) f = do
    (buf, consumed) <- applyBufferPool pool f
    if consumed < 0
        then do
            writeIORef ref buf
            return Nothing
        else do
            writeIORef ref $ BS.drop consumed buf
            return $ Just $ BS.take consumed buf

applyBufferPool
    :: BufferPool -> (Buffer -> BufSize -> IO Int) -> IO (ByteString, Int)
applyBufferPool (BufferPool l h ref) f = do
    buf0 <- readIORef ref
    buf <-
        if BS.length buf0 >= l
            then return buf0
            else mallocBS h
    consumed <- withForeignBuffer buf f
    return (buf, consumed)

withForeignBuffer :: ByteString -> (Buffer -> BufSize -> IO Int) -> IO Int
withForeignBuffer (PS ps s l) f = withForeignPtr ps $ \p -> f (castPtr p `plusPtr` s) l
{-# INLINE withForeignBuffer #-}

----------------------------------------------------------------

-- | Allocating a byte string.
mallocBS :: Int -> IO ByteString
mallocBS size = do
    ptr <- mallocBytes size
    fptr <- newForeignPtr finalizerFree ptr
    return $ PS fptr 0 size
{-# INLINE mallocBS #-}

-- | Copying the bytestring to the buffer.
--   This function returns the point where the next copy should start.
copy :: Buffer -> ByteString -> IO Buffer
copy ptr (PS fp o l) = withForeignPtr fp $ \p -> do
    copyBytes ptr (p `plusPtr` o) (fromIntegral l)
    return $ ptr `plusPtr` l
{-# INLINE copy #-}
