{-# LANGUAGE CPP, ForeignFunctionInterface #-}

module Network.Wai.Handler.Warp.SendFile (
    sendFile
  , readSendFile
  , packHeader -- for testing
  ) where

import Control.Monad (void)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Sendfile
import Network.Socket (Socket)
import Network.Wai.Handler.Warp.Buffer
import Network.Wai.Handler.Warp.Types

#ifdef WINDOWS
import Control.Monad (when)
import qualified System.IO as IO
import Foreign.Ptr (plusPtr)
#else
# if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
# endif
import Data.Word (Word8)
import Control.Exception
import Foreign.C.Types
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import System.Posix.IO (openFd, OpenFileFlags(..), defaultFileFlags, OpenMode(ReadOnly), closeFd)
import System.Posix.Types
#endif

----------------------------------------------------------------

-- | Function to send a file based on sendfiel() for Linux\/Mac\/FreeBSD.
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
      entire <- toBS buf siz
      send entire
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
        bs <- toBSD buf (hn + n)
        let n' = fromIntegral n
        send bs
        hook
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
    bs <- toBS buf (hn + n)
    let n' = fromIntegral n
    send bs
    hook
    loop fd (len0 - n') (off0 + n')
  where
    path = fileIdPath fid
    setup = case fileIdFd fid of
       Just fd -> return fd
       Nothing -> openFd path ReadOnly Nothing defaultFileFlags{nonBlock=True}
    teardown fd = case fileIdFd fid of
       Just _  -> return ()
       Nothing -> closeFd fd
    loop fd len off
      | len <= 0  = return ()
      | otherwise = do
          n <- positionRead fd buf (mini siz len) off
          bs <- toBS buf n
          let n' = fromIntegral n
          send bs
          hook
          loop fd (len - n') (off + n')

positionRead :: Fd -> Ptr Word8 -> Int -> Integer -> IO Int
positionRead (Fd fd) buf siz off =
    fromIntegral <$> c_pread fd (castPtr buf) (fromIntegral siz) (fromIntegral off)

foreign import ccall unsafe "pread"
  c_pread :: CInt -> Ptr CChar -> ByteCount -> FileOffset -> IO ByteCount -- fixme
#endif
