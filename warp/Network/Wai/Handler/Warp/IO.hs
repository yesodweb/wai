{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.IO where

import Blaze.ByteString.Builder.Internal.Types (Builder(..), BuildSignal(..), BufRange(..), runBuildStep, buildStep)
import Data.ByteString.Internal (ByteString(..))
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (plusPtr, minusPtr)
import Network.Wai.Handler.Warp.Buffer

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

toBS :: Buffer -> Int -> IO ByteString
toBS ptr siz = do
    fptr <- newForeignPtr_ ptr
    return $ PS fptr 0 siz
