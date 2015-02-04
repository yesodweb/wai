{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Types where

import Control.Exception
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.IORef (IORef, readIORef, writeIORef, newIORef)
import Data.Typeable (Typeable)
import Data.Word (Word16)
import Network.HTTP.Types.Header
import Network.Socket (Socket)
import Network.Wai.Handler.Warp.Buffer (Buffer,BufSize)
import qualified Network.Wai.Handler.Warp.Date as D
import qualified Network.Wai.Handler.Warp.FdCache as F
import qualified Network.Wai.Handler.Warp.Timeout as T

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

-- | Whether or not 'ConnSendFileOverride' in 'Connection' can be
--   overridden. This is a kind of hack to keep the signature of
--   'Connection' clean.
data ConnSendFileOverride = NotOverride     -- ^ Don't override
                          | Override Socket -- ^ Override with this 'Socket'

----------------------------------------------------------------

-- | Data type to manipulate IO actions for connections.
data Connection = Connection
    { connSendMany :: [ByteString] -> IO ()
    , connSendAll  :: ByteString -> IO ()
    , connSendFile :: FilePath -> Integer -> Integer -> IO () -> [ByteString] -> IO () -- ^ filepath, offset, length, hook action, HTTP headers
    , connClose    :: IO ()
    , connRecv     :: IO ByteString
    , connReadBuffer       :: Buffer
    , connWriteBuffer      :: Buffer
    , connBufferSize       :: BufSize
    , connSendFileOverride :: ConnSendFileOverride
    }

----------------------------------------------------------------

-- | Internal information.
data InternalInfo = InternalInfo {
    threadHandle :: T.Handle
  , fdCacher :: Maybe F.MutableFdCache
  , dateCacher :: D.DateCache
  }

----------------------------------------------------------------

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
