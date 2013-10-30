{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Types where

import Control.Exception
import Data.ByteString.Char8 (ByteString, pack)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Network.HTTP.Types.Header
import Network.Socket (Socket)
import qualified Network.Wai.Handler.Warp.FdCache as F
import qualified Network.Wai.Handler.Warp.Timeout as T
import qualified Paths_warp

----------------------------------------------------------------

-- | The version of Warp.
warpVersion :: String
warpVersion = showVersion Paths_warp.version

----------------------------------------------------------------

-- | TCP port number.
type Port = Int

----------------------------------------------------------------

-- | The type for header value used with 'HeaderName'.
type HeaderValue = ByteString

hTransferEncoding :: HeaderName
hTransferEncoding = "Transfer-Encoding"

hAcceptRanges :: HeaderName
hAcceptRanges = "Accept-Ranges"

hServer :: HeaderName
hServer = "Server"

defaultServerValue :: HeaderValue
defaultServerValue = pack $ "Warp/" ++ warpVersion

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
