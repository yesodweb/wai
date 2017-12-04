{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Network.Wai.Handler.Warp.SendFile (
    sendFile
  , readSendFile
  , packHeader -- for testing
#ifndef WINDOWS
  , positionRead
#endif
  ) where

import qualified Data.ByteString as BS
import Network.Socket (Socket)

#ifdef WINDOWS
import Foreign.ForeignPtr (newForeignPtr_)
import Foreign.Ptr (plusPtr)
import qualified System.IO as IO
#else
import Control.Exception
import Foreign.C.Error (throwErrno)
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Network.Sendfile
import Network.Wai.Handler.Warp.FdCache (openFile, closeFile)
import System.Posix.Types
#endif

import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | Function to send a file based on sendfile() for Linux\/Mac\/FreeBSD.
--   This makes use of the file descriptor cache.
--   For other OSes, this is identical to 'readSendFile'.
--
-- Since: 3.1.0
sendFile :: Socket -> Buffer -> BufSize -> (ByteString -> IO ()) -> SendFile
#ifdef SENDFILEFD
sendFile s _ _ _ fid off len act hdr = case mfid of
    -- settingsFdCacheDuration is 0
    Nothing -> sendfileWithHeader   s path (PartOfFile off len) act hdr
    Just fd -> sendfileFdWithHeader s fd   (PartOfFile off len) act hdr
  where
    mfid = fileIdFd fid
    path = fileIdPath fid
#else
sendFile _ = readSendFile
#endif

----------------------------------------------------------------

packHeader :: Buffer -> BufSize -> (ByteString -> IO ())
           -> IO () -> [ByteString]
           -> Int
           -> IO Int
packHeader _   _   _    _    [] n = return n
packHeader buf siz send hook (bs:bss) n
  | len < room = do
      let dst = buf `plusPtr` n
      void $ copy dst bs
      packHeader buf siz send hook bss (n + len)
  | otherwise  = do
      let dst = buf `plusPtr` n
          (bs1, bs2) = BS.splitAt room bs
      void $ copy dst bs1
      bufferIO buf siz send
      hook
      packHeader buf siz send hook (bs2:bss) 0
  where
    len = BS.length bs
    room = siz - n

mini :: Int -> Integer -> Int
mini i n
  | fromIntegral i < n = i
  | otherwise          = fromIntegral n

-- | Function to send a file based on pread()\/send() for Unix.
--   This makes use of the file descriptor cache.
--   For Windows, this is emulated by 'Handle'.
--
-- Since: 3.1.0
#ifdef WINDOWS
readSendFile :: Buffer -> BufSize -> (ByteString -> IO ()) -> SendFile
readSendFile buf siz send fid off0 len0 hook headers = do
    hn <- packHeader buf siz send hook headers 0
    let room = siz - hn
        buf' = buf `plusPtr` hn
    IO.withBinaryFile path IO.ReadMode $ \h -> do
        IO.hSeek h IO.AbsoluteSeek off0
        n <- IO.hGetBufSome h buf' (mini room len0)
        bufferIO buf (hn + n) send
        hook
        let n' = fromIntegral n
        fptr <- newForeignPtr_ buf
        loop h fptr (len0 - n')
  where
    path = fileIdPath fid
    loop h fptr len
      | len <= 0  = return ()
      | otherwise = do
        n <- IO.hGetBufSome h buf (mini siz len)
        when (n /= 0) $ do
            let bs = PS fptr 0 n
                n' = fromIntegral n
            send bs
            hook
            loop h fptr (len - n')
#else
readSendFile :: Buffer -> BufSize -> (ByteString -> IO ()) -> SendFile
readSendFile buf siz send fid off0 len0 hook headers =
  bracket setup teardown $ \fd -> do
    hn <- packHeader buf siz send hook headers 0
    let room = siz - hn
        buf' = buf `plusPtr` hn
    n <- positionRead fd buf' (mini room len0) off0
    bufferIO buf (hn + n) send
    hook
    let n' = fromIntegral n
    loop fd (len0 - n') (off0 + n')
  where
    path = fileIdPath fid
    setup = case fileIdFd fid of
       Just fd -> return fd
       Nothing -> openFile path
    teardown fd = case fileIdFd fid of
       Just _  -> return ()
       Nothing -> closeFile fd
    loop fd len off
      | len <= 0  = return ()
      | otherwise = do
          n <- positionRead fd buf (mini siz len) off
          bufferIO buf n send
          let n' = fromIntegral n
          hook
          loop fd (len - n') (off + n')

positionRead :: Fd -> Buffer -> BufSize -> Integer -> IO Int
positionRead fd buf siz off = do
    bytes <- fromIntegral <$> c_pread fd (castPtr buf) (fromIntegral siz) (fromIntegral off)
    when (bytes < 0) $ throwErrno "positionRead"
    return bytes

foreign import ccall unsafe "pread"
  c_pread :: Fd -> Ptr CChar -> ByteCount -> FileOffset -> IO CSsize
#endif
