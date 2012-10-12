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

-- |
--
-- In order to provide slowloris protection, Warp provides timeout handlers. We
-- follow these rules:
--
-- * A timeout is created when a connection is opened.
--
-- * When all request headers are read, the timeout is tickled.
--
-- * Every time at least 2048 bytes of the request body are read, the timeout
--   is tickled.
--
-- * The timeout is paused while executing user code. This will apply to both
--   the application itself, and a ResponseSource response. The timeout is
--   resumed as soon as we return from user code.
--
-- * Every time data is successfully sent to the client, the timeout is tickled.
data Connection = Connection
    { connSendMany :: [ByteString] -> IO ()
    , connSendAll  :: ByteString -> IO ()
    , connSendFile :: FilePath -> Integer -> Integer -> IO () -> [ByteString] -> Cleaner -> IO () -- ^ offset, length
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

data Cleaner = Cleaner {
    threadHandle :: T.Handle
  , fdCacher :: Maybe F.MutableFdCache
  }

#else
dummyCleaner = Cleaner T.dummyHandle

newtype Cleaner = Cleaner { threadHandle :: T.Handle }
#endif
