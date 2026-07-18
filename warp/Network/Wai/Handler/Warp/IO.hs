module Network.Wai.Handler.Warp.IO where

import Control.Exception (mask_)
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Extra (Next (Chunk, Done, More), runBuilder)
import Data.IORef (IORef, readIORef, writeIORef)
import Foreign.Ptr (plusPtr)
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

toBufIOWith
    :: Int -> IORef WriteBuffer -> (ByteString -> IO ()) -> Builder -> IO Integer
toBufIOWith = toBufIOWithOffset 0

-- | Like 'toBufIOWith' but the first @offset@ bytes of the write buffer
-- are assumed to be already filled (e.g. with a response header composed
-- directly into the buffer). They are flushed together with the first
-- batch of builder output and included in the returned total.
-- @offset@ must not exceed the current buffer size.
toBufIOWithOffset
    :: Int
    -> Int
    -> IORef WriteBuffer
    -> (ByteString -> IO ())
    -> Builder
    -> IO Integer
toBufIOWithOffset offset0 maxRspBufSize writeBufferRef io builder = do
    writeBuffer <- readIORef writeBufferRef
    loop writeBuffer offset0 firstWriter 0
  where
    firstWriter = runBuilder builder
    loop writeBuffer offset writer bytesSent = do
        let buf = bufBuffer writeBuffer
            size = bufSize writeBuffer
        (len, signal) <- writer (buf `plusPtr` offset) (size - offset)
        bufferIO writeBuffer (offset + len) io
        let totalBytesSent = toInteger (offset + len) + bytesSent
        case signal of
            Done -> return totalBytesSent
            More minSize next
                | size < minSize -> do
                    when (minSize > maxRspBufSize) $
                        error $
                            "Sending a Builder response required a buffer of size "
                                ++ show minSize
                                ++ " which is bigger than the specified maximum of "
                                ++ show maxRspBufSize
                                ++ "!"
                    -- The current WriteBuffer is too small to fit the next
                    -- batch of bytes from the Builder so we free it and
                    -- create a new bigger one. Freeing the current buffer,
                    -- creating a new one and writing it to the IORef need
                    -- to be performed atomically to prevent both double
                    -- frees and missed frees. So we mask async exceptions:
                    biggerWriteBuffer <- mask_ $ do
                        bufFree writeBuffer
                        biggerWriteBuffer <- createWriteBuffer minSize
                        writeIORef writeBufferRef biggerWriteBuffer
                        return biggerWriteBuffer
                    loop biggerWriteBuffer 0 next totalBytesSent
                | otherwise -> loop writeBuffer 0 next totalBytesSent
            Chunk bs next -> do
                io bs
                loop writeBuffer 0 next totalBytesSent
