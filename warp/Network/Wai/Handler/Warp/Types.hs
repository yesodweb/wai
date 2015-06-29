{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Types where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Typeable (Typeable)
import Data.Word (Word16, Word8)
import Foreign.Ptr (Ptr)
import Network.HTTP.Types.Header
import qualified Network.Wai.Handler.Warp.Date as D
import qualified Network.Wai.Handler.Warp.FdCache as F
import qualified Network.Wai.Handler.Warp.Timeout as T

#ifndef WINDOWS
import System.Posix.Types (Fd)
#endif

----------------------------------------------------------------

-- | TCP port number.
type Port = Int

----------------------------------------------------------------

-- | The type for header value used with 'HeaderName'.
type HeaderValue = ByteString

hTransferEncoding :: HeaderName
hTransferEncoding = "Transfer-Encoding"

hContentRange :: HeaderName
hContentRange = "Content-Range"

hAcceptRanges :: HeaderName
hAcceptRanges = "Accept-Ranges"

hServer :: HeaderName
hServer = "Server"

----------------------------------------------------------------

-- | Error types for bad 'Request'.
data InvalidRequest = NotEnoughLines [String]
                    | BadFirstLine String
                    | NonHttp
                    | IncompleteHeaders
                    | ConnectionClosedByPeer
                    | OverLargeHeader
                    | BadProxyHeader String
                    deriving (Eq, Typeable)

instance Show InvalidRequest where
    show (NotEnoughLines xs) = "Warp: Incomplete request headers, received: " ++ show xs
    show (BadFirstLine s) = "Warp: Invalid first line of request: " ++ show s
    show NonHttp = "Warp: Request line specified a non-HTTP request"
    show IncompleteHeaders = "Warp: Request headers did not finish transmission"
    show ConnectionClosedByPeer = "Warp: Client closed connection prematurely"
    show OverLargeHeader = "Warp: Request headers too large, possible memory attack detected. Closing connection."
    show (BadProxyHeader s) = "Warp: Invalid PROXY protocol header: " ++ show s

instance Exception InvalidRequest

----------------------------------------------------------------

#ifdef WINDOWS
type Fd = ()
#endif

-- | Data type to abstract file identifiers.
--   On Unix, a file descriptor would be specified to make use of
--   the file descriptor cache.
--
-- Since: 3.1.0
data FileId = FileId {
    fileIdPath :: FilePath
  , fileIdFd   :: Maybe Fd
  }

-- |  fileid, offset, length, hook action, HTTP headers
--
-- Since: 3.1.0
type SendFile = FileId -> Integer -> Integer -> IO () -> [ByteString] -> IO ()

-- | Type for read buffer pool
type BufferPool = IORef ByteString

-- | Type for buffer
type Buffer = Ptr Word8

-- | Type for buffer size
type BufSize = Int

-- | Type for the action to receive input data
type Recv = IO ByteString

-- | Type for the action to receive input data with a buffer.
--   The result boolean indicates whether or not the buffer is fully filled.
type RecvBuf = Buffer -> BufSize -> IO Bool

-- | Data type to manipulate IO actions for connections.
--   This is used to abstract IO actions for plain HTTP and HTTP over TLS.
data Connection = Connection {
    -- | This is not used at this moment.
      connSendMany    :: [ByteString] -> IO ()
    -- | The sending function.
    , connSendAll     :: ByteString -> IO ()
    -- | The sending function for files in HTTP/1.1.
    , connSendFile    :: SendFile
    -- | The connection closing function.
    , connClose       :: IO ()
    -- | The connection receiving function. This returns "" for EOF.
    , connRecv        :: Recv
    -- | The connection receiving function. This tries to fill the buffer.
    --   This returns when the buffer is filled or reaches EOF.
    , connRecvBuf     :: RecvBuf
    -- | The write buffer.
    , connWriteBuffer :: Buffer
    -- | The size of the write buffer.
    , connBufferSize  :: BufSize
    }

----------------------------------------------------------------

-- | Internal information.
data InternalInfo = InternalInfo {
    threadHandle :: T.Handle
  , timeoutManager :: T.Manager
  , fdCacher :: Maybe F.MutableFdCache
  , dateCacher :: D.DateCache
  }

----------------------------------------------------------------

-- | Type for input streaming.
data Source = Source !(IORef ByteString) !(IO ByteString)

mkSource :: IO ByteString -> IO Source
mkSource func = do
    ref <- newIORef S.empty
    return $! Source ref func

readSource :: Source -> IO ByteString
readSource (Source ref func) = do
    bs <- readIORef ref
    if S.null bs
        then func
        else do
            writeIORef ref S.empty
            return bs

-- | Read from a Source, ignoring any leftovers.
readSource' :: Source -> IO ByteString
readSource' (Source _ func) = func

leftoverSource :: Source -> ByteString -> IO ()
leftoverSource (Source ref _) bs = writeIORef ref bs

readLeftoverSource :: Source -> IO ByteString
readLeftoverSource (Source ref _) = readIORef ref

----------------------------------------------------------------

-- | What kind of transport is used for this connection?
data Transport = TCP -- ^ Plain channel: TCP
               | TLS {
                   tlsMajorVersion :: Int
                 , tlsMinorVersion :: Int
                 , tlsNegotiatedProtocol :: Maybe ByteString -- ^ The result of Application Layer Protocol Negociation in RFC 7301
                 , tlsChiperID :: Word16
                 }  -- ^ Encrypted channel: TLS or SSL

isTransportSecure :: Transport -> Bool
isTransportSecure TCP = False
isTransportSecure _   = True
