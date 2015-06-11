{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Network.Wai.Handler.Warp.SendFile where

import Data.ByteString.Internal
import Data.Word (Word8)
import Foreign.ForeignPtr (newForeignPtr_, withForeignPtr)
import Network.Sendfile
import Network.Socket (Socket)
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Types

#ifdef WINDOWS
import Control.Monad (when, void)
import qualified System.IO as IO
#else
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import System.Posix.Types
#endif

----------------------------------------------------------------

defaultSendFile :: Socket -> Buffer -> BufSize -> (ByteString -> IO ()) -> SendFile
#ifdef SENDFILEFD
defaultSendFile s _ _ _ fid off len act hdr = case mfid of
    Just fd -> sendfileFdWithHeader s fd   (PartOfFile off len) act hdr
    -- never reached, just in case
    Nothing -> sendfileWithHeader   s path (PartOfFile off len) act hdr
  where
    path = fileIdPath fid
    mfid = fileIdFd fid
#else
defaultSendFile _ = readSendFile
#endif

----------------------------------------------------------------

packHeader :: Buffer -> BufSize -> (ByteString -> IO ())
           -> IO () -> [ByteString]
           -> Int
           -> IO Int
packHeader _   _   _    _    [] n = return n
packHeader buf siz send hook (PS bsfp off len : bss) n
  | len < room = withForeignPtr bsfp $ \bsp -> do
      let dst = buf `plusPtr` n
          src = bsp `plusPtr` off
      memcpy dst src len
      packHeader buf siz send hook bss (n + len)
  | otherwise  = withForeignPtr bsfp $ \bsp -> do
      let dst = buf `plusPtr` n
          src = bsp `plusPtr` off
      memcpy dst src room
      fptr <- newForeignPtr_ buf
      let entire = PS fptr 0 siz
          bs = PS bsfp (off + room) (len - room)
      send entire
      hook
      packHeader buf siz send hook (bs:bss) 0
  where
    room = siz - n

#ifdef WINDOWS
readSendFile :: Buffer -> BufSize -> (ByteString -> IO ()) -> SendFile
readSendFile buf siz send fid off0 len0 hook headers = do
    fptr <- newForeignPtr_ buf
    hn <- packHeader buf siz send hook headers 0
    let room = siz - hn
        buf' = buf `plusPtr` hn
    IO.withBinaryFile path IO.ReadMode $ \h -> do
        IO.hSeek h IO.AbsoluteSeek off0
        n <- IO.hGetBufSome h buf' room
        let bs = PS fptr 0 (hn + n)
            len = len0 - fromIntegral n
        send bs
        hook
        loop fptr h len
  where
    path = fileIdPath fid
    loop fptr h len
      | len <= 0  = return ()
      | otherwise = do
        n <- IO.hGetBufSome h buf siz
        when (n /= 0) $ do
            let bs = PS fptr 0 n
            send bs
            void $ hook
            loop fptr h $ len - fromIntegral n
#else
readSendFile :: Buffer -> BufSize -> (ByteString -> IO ()) -> SendFile
readSendFile buf siz send fid off0 len0 hook headers = do
    fptr <- newForeignPtr_ buf
    hn <- packHeader buf siz send hook headers 0
    let room = siz - hn
        buf' = buf `plusPtr` hn
    n <- positionRead fd buf' (mini room len0) off0
    let bs = PS fptr 0 (hn + n)
        n' = fromIntegral n
    send bs
    hook
    loop fptr (len0 - n') (off0 + n')
  where
    Just fd = fileIdFd fid -- fixme
    mini i n
      | fromIntegral i < n = i
      | otherwise          = fromIntegral n
    loop fptr len off
      | len <= 0  = return ()
      | otherwise = do
          n <- positionRead fd buf (mini siz len) off
          let bs = PS fptr 0 n
              n' = fromIntegral n
          send bs
          hook
          loop fptr (len - n') (off + n')

positionRead :: Fd -> Ptr Word8 -> Int -> Integer -> IO Int
positionRead (Fd fd) buf siz off =
    fromIntegral <$> c_pread fd (castPtr buf) (fromIntegral siz) (fromIntegral off)

foreign import ccall unsafe "pread"
  c_pread :: CInt -> Ptr CChar -> ByteCount -> FileOffset -> IO ByteCount -- fixme
#endif
