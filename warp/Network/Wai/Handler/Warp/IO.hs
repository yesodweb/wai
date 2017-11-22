{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.IO where

import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder.Extra (runBuilder, Next(Done, More, Chunk))

import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

toBufIOWith :: Buffer -> BufSize -> (ByteString -> IO ()) -> Builder -> IO ()
toBufIOWith buf !size io builder = loop firstWriter
  where
    firstWriter = runBuilder builder
    runIO len = bufferIO buf len io
    loop writer = do
        (len, signal) <- writer buf size
        case signal of
             Done -> runIO len
             More minSize next
               | size < minSize -> error "toBufIOWith: BufferFull: minSize"
               | otherwise      -> do
                   runIO len
                   loop next
             Chunk bs next -> do
                 runIO len
                 io bs
                 loop next
