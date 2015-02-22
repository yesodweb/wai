{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.IO where

import Data.ByteString.Internal (ByteString(..))
import Foreign.ForeignPtr (newForeignPtr_)
import Network.Wai.Handler.Warp.Buffer

-- Support for doctest, where cabal macros are not available
#ifndef MIN_VERSION_blaze_builder
#define MIN_VERSION_blaze_builder(x, y, z) 1
#endif

#if MIN_VERSION_blaze_builder(0,4,0)

import Blaze.ByteString.Builder (Builder)
import Data.ByteString.Builder.Extra (runBuilder, Next(Done, More, Chunk))

toBufIOWith :: Buffer -> BufSize -> (ByteString -> IO ()) -> Builder -> IO ()
toBufIOWith buf !size io builder = loop firstWriter
  where
    firstWriter = runBuilder builder
    runIO len = toBS buf len >>= io
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

#else /* !MIN_VERSION_blaze_builder(0,4,0) */

import Blaze.ByteString.Builder.Internal.Types (Builder(..), BuildSignal(..), BufRange(..), runBuildStep, buildStep)
import Foreign.Ptr (plusPtr, minusPtr)

toBufIOWith :: Buffer -> BufSize -> (ByteString -> IO ()) -> Builder -> IO ()
toBufIOWith buf !size io (Builder build) = loop firstStep
  where
    firstStep = build (buildStep finalStep)
    finalStep (BufRange p _) = return $ Done p ()
    bufRange = BufRange buf (buf `plusPtr` size)
    runIO ptr = toBS buf (ptr `minusPtr` buf) >>= io
    loop step = do
        signal <- runBuildStep step bufRange
        case signal of
             Done ptr _ -> runIO ptr
             BufferFull minSize ptr next
               | size < minSize -> error "toBufIOWith: BufferFull: minSize"
               | otherwise      -> do
                   runIO ptr
                   loop next
             InsertByteString ptr bs next -> do
                 runIO ptr
                 io bs
                 loop next

#endif /* !MIN_VERSION_blaze_builder(0,4,0) */

toBS :: Buffer -> Int -> IO ByteString
toBS ptr siz = do
    fptr <- newForeignPtr_ ptr
    return $ PS fptr 0 siz
