{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Types where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Network.HTTP.Types.Header
import Network.Socket (Socket)
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
                    deriving (Eq, Show, Typeable)

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
    , connSendFileOverride :: ConnSendFileOverride
    }

----------------------------------------------------------------

-- | Internal information.
data InternalInfo = InternalInfo {
    threadHandle :: T.Handle
  , fdCacher :: Maybe F.MutableFdCache
  }
