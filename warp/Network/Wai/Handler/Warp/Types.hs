{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Types where

import Control.Exception
import Data.ByteString (ByteString)
import Data.Typeable (Typeable)
import Data.Version (showVersion)
import Network.HTTP.Types.Header
import qualified Paths_warp
import qualified Network.Wai.Handler.Warp.Timeout as T
#if SENDFILEFD
import qualified Network.Wai.Handler.Warp.FdCache as F
#endif

----------------------------------------------------------------

warpVersion :: String
warpVersion = showVersion Paths_warp.version

----------------------------------------------------------------

-- | TCP port number
type Port = Int

----------------------------------------------------------------

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

-- | Data type to manipulate IO actions for connections.
data Connection = Connection
    { connSendMany :: [ByteString] -> IO ()
    , connSendAll  :: ByteString -> IO ()
    , connSendFile :: FilePath -> Integer -> Integer -> IO () -> [ByteString] -> Cleaner -> IO () -- ^ filepath, offset, length, hook action, HTTP headers, fd clear
    , connClose    :: IO ()
    , connRecv     :: IO ByteString
    }

----------------------------------------------------------------

-- | A dummy @Cleaner@, intended for applications making use of the low-level
-- request parsing and rendering functions.
--
-- Since 1.3.4
dummyCleaner :: Cleaner

#if SENDFILEFD
dummyCleaner = Cleaner T.dummyHandle Nothing

-- | A type used to clean up file descriptor caches.
data Cleaner = Cleaner {
    threadHandle :: T.Handle
  , fdCacher :: Maybe F.MutableFdCache
  }

#else
dummyCleaner = Cleaner T.dummyHandle

newtype Cleaner = Cleaner { threadHandle :: T.Handle }
#endif
