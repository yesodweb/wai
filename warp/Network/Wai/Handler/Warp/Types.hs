{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Types where

import qualified UnliftIO
import qualified Data.ByteString as S
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Typeable (Typeable)
#ifdef MIN_VERSION_x509
import Data.X509
#endif
import Network.Socket.BufferPool
import System.Posix.Types (Fd)
import qualified System.TimeManager as T

import qualified Network.Wai.Handler.Warp.Date as D
import qualified Network.Wai.Handler.Warp.FdCache as F
import qualified Network.Wai.Handler.Warp.FileInfoCache as I
import Network.Wai.Handler.Warp.Imports

----------------------------------------------------------------

-- | TCP port number.
type Port = Int

----------------------------------------------------------------

-- | The type for header value used with 'HeaderName'.
type HeaderValue = ByteString

----------------------------------------------------------------

-- | Error types for bad 'Request'.
data InvalidRequest = NotEnoughLines [String]
                    | BadFirstLine String
                    | NonHttp
                    | IncompleteHeaders
                    | ConnectionClosedByPeer
                    | OverLargeHeader
                    | BadProxyHeader String
                    | PayloadTooLarge -- ^ Since 3.3.22
                    | RequestHeaderFieldsTooLarge -- ^ Since 3.3.22
                    deriving (Eq, Typeable)

instance Show InvalidRequest where
    show (NotEnoughLines xs) = "Warp: Incomplete request headers, received: " ++ show xs
    show (BadFirstLine s) = "Warp: Invalid first line of request: " ++ show s
    show NonHttp = "Warp: Request line specified a non-HTTP request"
    show IncompleteHeaders = "Warp: Request headers did not finish transmission"
    show ConnectionClosedByPeer = "Warp: Client closed connection prematurely"
    show OverLargeHeader = "Warp: Request headers too large, possible memory attack detected. Closing connection."
    show (BadProxyHeader s) = "Warp: Invalid PROXY protocol header: " ++ show s
    show RequestHeaderFieldsTooLarge = "Request header fields too large"
    show PayloadTooLarge = "Payload too large"

instance UnliftIO.Exception InvalidRequest

----------------------------------------------------------------

-- | Exception thrown if something goes wrong while in the midst of
-- sending a response, since the status code can't be altered at that
-- point.
--
-- Used to determine whether keeping the HTTP1.1 connection / HTTP2 stream alive is safe
-- or irrecoverable.

newtype ExceptionInsideResponseBody = ExceptionInsideResponseBody UnliftIO.SomeException
    deriving (Show, Typeable)

instance UnliftIO.Exception ExceptionInsideResponseBody

----------------------------------------------------------------

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

-- | A write buffer of a specified size
-- containing bytes and a way to free the buffer.
data WriteBuffer = WriteBuffer {
      bufBuffer :: Buffer
      -- | The size of the write buffer.
    , bufSize :: !BufSize
      -- | Free the allocated buffer. Warp guarantees it will only be
      -- called once, and no other functions will be called after it.
    , bufFree :: IO ()
    }

-- | Data type to manipulate IO actions for connections.
--   This is used to abstract IO actions for plain HTTP and HTTP over TLS.
data Connection = Connection {
    -- | This is not used at this moment.
      connSendMany    :: [ByteString] -> IO ()
    -- | The sending function.
    , connSendAll     :: ByteString -> IO ()
    -- | The sending function for files in HTTP/1.1.
    , connSendFile    :: SendFile
    -- | The connection closing function. Warp guarantees it will only be
    -- called once. Other functions (like 'connRecv') may be called after
    -- 'connClose' is called.
    , connClose       :: IO ()
    -- | The connection receiving function. This returns "" for EOF or exceptions.
    , connRecv        :: Recv
    -- | The connection receiving function. This tries to fill the buffer.
    --   This returns when the buffer is filled or reaches EOF.
    , connRecvBuf     :: RecvBuf
    -- | Reference to a write buffer. When during sending of a 'Builder'
    -- response it's detected the current 'WriteBuffer' is too small it will be
    -- freed and a new bigger buffer will be created and written to this
    -- reference.
    , connWriteBuffer :: IORef WriteBuffer
    -- | Is this connection HTTP/2?
    , connHTTP2       :: IORef Bool
    }

getConnHTTP2 :: Connection -> IO Bool
getConnHTTP2 conn = readIORef (connHTTP2 conn)

setConnHTTP2 :: Connection -> Bool -> IO ()
setConnHTTP2 conn b = writeIORef (connHTTP2 conn) b

----------------------------------------------------------------

data InternalInfo = InternalInfo {
    timeoutManager :: T.Manager
  , getDate        :: IO D.GMTDate
  , getFd          :: FilePath -> IO (Maybe F.Fd, F.Refresh)
  , getFileInfo    :: FilePath -> IO I.FileInfo
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
#ifdef MIN_VERSION_x509
                 , tlsClientCertificate :: Maybe CertificateChain
#endif
                 }  -- ^ Encrypted channel: TLS or SSL
               | QUIC {
                   quicNegotiatedProtocol :: Maybe ByteString
                 , quicChiperID :: Word16
#ifdef MIN_VERSION_x509
                 , quicClientCertificate :: Maybe CertificateChain
#endif
                 }

isTransportSecure :: Transport -> Bool
isTransportSecure TCP = False
isTransportSecure _   = True

isTransportQUIC :: Transport -> Bool
isTransportQUIC QUIC{} = True
isTransportQUIC _      = False

#ifdef MIN_VERSION_x509
getTransportClientCertificate :: Transport -> Maybe CertificateChain
getTransportClientCertificate TCP              = Nothing
getTransportClientCertificate (TLS _ _ _ _ cc) = cc
getTransportClientCertificate (QUIC _ _ cc)    = cc
#endif
