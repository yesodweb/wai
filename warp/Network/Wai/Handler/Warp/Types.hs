{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Types where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Network.HTTP.Types.Header
import Network.Socket (Socket)
import qualified Network.Wai.Handler.Warp.FdCache as F
import qualified Network.Wai.Handler.Warp.Timeout as T
import qualified Paths_warp

----------------------------------------------------------------

warpVersion :: String
warpVersion = showVersion Paths_warp.version

----------------------------------------------------------------

-- | TCP port number
type Port = Int

----------------------------------------------------------------

type HeaderValue = ByteString

hTransferEncoding :: HeaderName
hTransferEncoding = "Transfer-Encoding"

hHost :: HeaderName
hHost = "Host"

hServer :: HeaderName
hServer = "Server"

----------------------------------------------------------------

-- | Error types for bad 'Request'.
data InvalidRequest =
    NotEnoughLines [String]
    | BadFirstLine String
    | NonHttp
    | IncompleteHeaders
    | ConnectionClosedByPeer
    | OverLargeHeader
    deriving (Eq, Show, Typeable)

instance Exception InvalidRequest

----------------------------------------------------------------

data ConnSendFileOverride = NotOverride | Override Socket

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
