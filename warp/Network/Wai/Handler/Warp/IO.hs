{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.IO where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Extra (Next (Chunk, Done, More), runBuilder)
import Data.IORef (IORef, readIORef, writeIORef)
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

toBufIOWith :: IORef WriteBuffer -> (ByteString -> IO ()) -> Builder -> IO ()
toBufIOWith writeBufferRef io builder = do
  writeBuffer <- readIORef writeBufferRef
  loop writeBuffer firstWriter
  where
    firstWriter = runBuilder builder
    loop writeBuffer writer = do
      let size = bufSize writeBuffer
          buf = bufBytes writeBuffer
          runIO len = bufferIO buf len io
      (len, signal) <- writer buf size
      case signal of
        Done -> runIO len
        More minSize next
          | size < minSize -> do
              bufFree writeBuffer
              writeBuffer' <- createWriteBuffer minSize
              writeIORef writeBufferRef writeBuffer'
              loop writeBuffer' next
          | otherwise -> do
              runIO len
              loop writeBuffer next
        Chunk bs next -> do
          runIO len
          io bs
          loop writeBuffer next
