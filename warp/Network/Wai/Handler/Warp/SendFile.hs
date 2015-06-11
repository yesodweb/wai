{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.SendFile where

import Control.Monad (when, void)
import Data.ByteString.Internal
import Foreign.ForeignPtr (newForeignPtr_)
import Network.Sendfile
import Network.Socket (Socket)
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Types
import qualified System.IO as IO

defaultSendFile :: Socket -> SendFile
#ifdef SENDFILEFD
defaultSendFile s fid off len act hdr = case mfid of
    Nothing -> sendfileWithHeader   s path (PartOfFile off len) act hdr
    Just fd -> sendfileFdWithHeader s fd   (PartOfFile off len) act hdr
  where
    path = fileIdPath fid
    mfid = fileIdFd fid
#else
defaultSendFile s fid off len act hdr =
    sendfileWithHeader s path (PartOfFile off len) act hdr
  where
    path = fileIdPath fid
#endif

readSendFile :: Buffer -> BufSize -> (ByteString -> IO ()) -> SendFile
readSendFile buf siz send fid offset len hook headers = do
    mapM_ send headers
    fptr <- newForeignPtr_ buf
    IO.withBinaryFile path IO.ReadMode $ \h -> do
        IO.hSeek h IO.AbsoluteSeek offset
        loop fptr h $ fromIntegral len
  where
    path = fileIdPath fid
    loop _    _ remaining | remaining <= 0 = return ()
    loop fptr h remaining = do
        n <- IO.hGetBufSome h buf siz
        when (n /= 0) $ do
            let bs = PS fptr 0 n
            send bs
            void $ hook
            loop fptr h $ remaining - n
