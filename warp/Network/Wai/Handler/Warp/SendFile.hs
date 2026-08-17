{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Network.Wai.Handler.Warp.SendFile (
    sendFile,
    readSendFile,
    packHeader, -- for testing
#ifndef WINDOWS
    positionRead,
#endif
) where

import qualified Data.ByteString as BS
import Network.Socket (Socket)
import Network.Socket.BufferPool

#ifdef WINDOWS
import Foreign.Ptr (plusPtr)
import qualified System.IO as IO
#else
import qualified Control.Exception as E
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
sendFile :: Socket -> WriteBuffer -> (ByteString -> IO ()) -> SendFile
#ifdef SENDFILEFD
sendFile s _ _ fid off len act hdr = case mfid of
    -- settingsFdCacheDuration is 0
    Nothing -> sendfileWithHeader s path (PartOfFile off len) act hdr
    Just fd -> sendfileFdWithHeader s fd (PartOfFile off len) act hdr
  where
    mfid = fileIdFd fid
    path = fileIdPath fid
#else
sendFile _ = readSendFile
#endif

----------------------------------------------------------------

packHeader
    :: WriteBuffer
    -> (ByteString -> IO ())
    -> IO ()
    -> [ByteString]
    -> Int
    -> IO Int
packHeader _ _ _ [] n = return n
packHeader wbuf send hook (bs : bss) n
    | len < room = do
        let dst = bufBuffer wbuf `plusPtr` n
        void $ copy dst bs
        packHeader wbuf send hook bss (n + len)
    | otherwise = do
        let dst = bufBuffer wbuf `plusPtr` n
            (bs1, bs2) = BS.splitAt room bs
        void $ copy dst bs1
        bufferIO wbuf (bufSize wbuf) send
        hook
        packHeader wbuf send hook (bs2 : bss) 0
  where
    len = BS.length bs
    room = bufSize wbuf - n

mini :: Int -> Integer -> Int
mini i n
    | fromIntegral i < n = i
    | otherwise = fromIntegral n

-- | Function to send a file based on pread()\/send() for Unix.
--   This makes use of the file descriptor cache.
--   For Windows, this is emulated by 'Handle'.
--
-- Since: 3.1.0
#ifdef WINDOWS
readSendFile :: WriteBuffer -> (ByteString -> IO ()) -> SendFile
readSendFile wbuf send fid off0 len0 hook headers = do
    hn <- packHeader wbuf send hook headers 0
    let room = siz - hn
        buf' = buf `plusPtr` hn
    IO.withBinaryFile path IO.ReadMode $ \h -> do
        IO.hSeek h IO.AbsoluteSeek off0
        n <- IO.hGetBufSome h buf' (mini room len0)
        bufferIO wbuf (hn + n) send
        hook
        let n' = fromIntegral n
        loop h (len0 - n')
  where
    buf = bufBuffer wbuf
    siz = bufSize wbuf
    path = fileIdPath fid
    loop h len
        | len <= 0 = return ()
        | otherwise = do
            n <- IO.hGetBufSome h buf (mini siz len)
            when (n /= 0) $ do
                bufferIO wbuf n send
                hook
                loop h (len - fromIntegral n)
#else
readSendFile :: WriteBuffer -> (ByteString -> IO ()) -> SendFile
readSendFile wbuf send fid off0 len0 hook headers =
    E.bracket setup teardown $ \fd -> do
        hn <- packHeader wbuf send hook headers 0
        let room = siz - hn
            buf' = buf `plusPtr` hn
        n <- positionRead fd buf' (mini room len0) off0
        bufferIO wbuf (hn + n) send
        hook
        let n' = fromIntegral n
        loop fd (len0 - n') (off0 + n')
  where
    buf = bufBuffer wbuf
    siz = bufSize wbuf
    path = fileIdPath fid
    setup = case fileIdFd fid of
        Just fd -> return fd
        Nothing -> openFile path
    teardown fd = case fileIdFd fid of
        Just _ -> return ()
        Nothing -> closeFile fd
    loop fd len off
        | len <= 0 = return ()
        | otherwise = do
            n <- positionRead fd buf (mini siz len) off
            bufferIO wbuf n send
            let n' = fromIntegral n
            hook
            loop fd (len - n') (off + n')

positionRead :: Fd -> Buffer -> BufSize -> Integer -> IO Int
positionRead fd buf siz off = do
    bytes <-
        fromIntegral <$> c_pread fd (castPtr buf) (fromIntegral siz) (fromIntegral off)
    when (bytes < 0) $ throwErrno "positionRead"
    return bytes

foreign import ccall unsafe "pread"
    c_pread :: Fd -> Ptr CChar -> ByteCount -> FileOffset -> IO CSsize
#endif
