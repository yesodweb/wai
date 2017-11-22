{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, ViewPatterns #-}
{-# LANGUAGE PatternGuards, RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes, CPP #-}

module Network.Wai.Handler.Warp.Settings where

import Control.Concurrent (forkIOWithUnmask)
import Control.Exception
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as C8
import Data.Streaming.Network (HostPreference)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Version (showVersion)
import GHC.IO.Exception (IOErrorType(..))
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)
import Network.Wai
import qualified Paths_warp
import System.IO (stderr)
import System.IO.Error (ioeGetErrorType)

import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Timeout
import Network.Wai.Handler.Warp.Types

-- | Various Warp server settings. This is purposely kept as an abstract data
-- type so that new settings can be added without breaking backwards
-- compatibility. In order to create a 'Settings' value, use 'defaultSettings'
-- and the various \'set\' functions to modify individual fields. For example:
--
-- > setTimeout 20 defaultSettings
data Settings = Settings
    { settingsPort :: Port -- ^ Port to listen on. Default value: 3000
    , settingsHost :: HostPreference -- ^ Default value: HostIPv4
    , settingsOnException :: Maybe Request -> SomeException -> IO () -- ^ What to do with exceptions thrown by either the application or server. Default: ignore server-generated exceptions (see 'InvalidRequest') and print application-generated applications to stderr.
    , settingsOnExceptionResponse :: SomeException -> Response
      -- ^ A function to create `Response` when an exception occurs.
      --
      -- Default: 500, text/plain, \"Something went wrong\"
      --
      -- Since 2.0.3
    , settingsOnOpen :: SockAddr -> IO Bool -- ^ What to do when a connection is open. When 'False' is returned, the connection is closed immediately. Otherwise, the connection is going on. Default: always returns 'True'.
    , settingsOnClose :: SockAddr -> IO ()  -- ^ What to do when a connection is close. Default: do nothing.
    , settingsTimeout :: Int -- ^ Timeout value in seconds. Default value: 30
    , settingsManager :: Maybe Manager -- ^ Use an existing timeout manager instead of spawning a new one. If used, 'settingsTimeout' is ignored. Default is 'Nothing'
    , settingsFdCacheDuration :: Int -- ^ Cache duration time of file descriptors in seconds. 0 means that the cache mechanism is not used. Default value: 0
    , settingsFileInfoCacheDuration :: Int -- ^ Cache duration time of file information in seconds. 0 means that the cache mechanism is not used. Default value: 0
    , settingsBeforeMainLoop :: IO ()
      -- ^ Code to run after the listening socket is ready but before entering
      -- the main event loop. Useful for signaling to tests that they can start
      -- running, or to drop permissions after binding to a restricted port.
      --
      -- Default: do nothing.
      --
      -- Since 1.3.6

    , settingsFork :: ((forall a. IO a -> IO a) -> IO ()) -> IO ()
      -- ^ Code to fork a new thread to accept a connection.
      --
      -- This may be useful if you need OS bound threads, or if
      -- you wish to develop an alternative threading model.
      --
      -- Default: void . forkIOWithUnmask
      --
      -- Since 3.0.4

    , settingsNoParsePath :: Bool
      -- ^ Perform no parsing on the rawPathInfo.
      --
      -- This is useful for writing HTTP proxies.
      --
      -- Default: False
      --
      -- Since 2.0.3
    , settingsInstallShutdownHandler :: IO () -> IO ()
    , settingsServerName :: ByteString
      -- ^ Default server name if application does not set one.
      --
      -- Since 3.0.2
    , settingsMaximumBodyFlush :: Maybe Int
      -- ^ See @setMaximumBodyFlush@.
      --
      -- Since 3.0.3
    , settingsProxyProtocol :: ProxyProtocol
      -- ^ Specify usage of the PROXY protocol.
      --
      -- Since 3.0.5.
    , settingsSlowlorisSize :: Int
      -- ^ Size of bytes read to prevent Slowloris protection. Default value: 2048
      --
      -- Since 3.1.2.
    , settingsHTTP2Enabled :: Bool
      -- ^ Whether to enable HTTP2 ALPN/upgrades. Default: True
      --
      -- Since 3.1.7.
    , settingsLogger :: Request -> H.Status -> Maybe Integer -> IO ()
      -- ^ A log function. Default: no action.
      --
      -- Since 3.X.X.
    , settingsServerPushLogger :: Request -> ByteString -> Integer -> IO ()
      -- ^ A HTTP/2 server push log function. Default: no action.
      --
      -- Since 3.X.X.
    , settingsGracefulShutdownTimeout :: Maybe Int
      -- ^ An optional timeout to limit the time (in seconds) waiting for
      -- a graceful shutdown of the web server.
      --
      -- Since 3.2.8
    }

-- | Specify usage of the PROXY protocol.
data ProxyProtocol = ProxyProtocolNone
                     -- ^ See @setProxyProtocolNone@.
                   | ProxyProtocolRequired
                     -- ^ See @setProxyProtocolRequired@.
                   | ProxyProtocolOptional
                     -- ^ See @setProxyProtocolOptional@.

-- | The default settings for the Warp server. See the individual settings for
-- the default value.
defaultSettings :: Settings
defaultSettings = Settings
    { settingsPort = 3000
    , settingsHost = "*4"
    , settingsOnException = defaultOnException
    , settingsOnExceptionResponse = defaultOnExceptionResponse
    , settingsOnOpen = const $ return True
    , settingsOnClose = const $ return ()
    , settingsTimeout = 30
    , settingsManager = Nothing
    , settingsFdCacheDuration = 0
    , settingsFileInfoCacheDuration = 0
    , settingsBeforeMainLoop = return ()
    , settingsFork = void . forkIOWithUnmask
    , settingsNoParsePath = False
    , settingsInstallShutdownHandler = const $ return ()
    , settingsServerName = C8.pack $ "Warp/" ++ showVersion Paths_warp.version
    , settingsMaximumBodyFlush = Just 8192
    , settingsProxyProtocol = ProxyProtocolNone
    , settingsSlowlorisSize = 2048
    , settingsHTTP2Enabled = True
    , settingsLogger = \_ _ _ -> return ()
    , settingsServerPushLogger = \_ _ _ -> return ()
    , settingsGracefulShutdownTimeout = Nothing
    }

-- | Apply the logic provided by 'defaultOnException' to determine if an
-- exception should be shown or not. The goal is to hide exceptions which occur
-- under the normal course of the web server running.
--
-- Since 2.1.3
defaultShouldDisplayException :: SomeException -> Bool
defaultShouldDisplayException se
    | Just ThreadKilled <- fromException se = False
    | Just (_ :: InvalidRequest) <- fromException se = False
    | Just (ioeGetErrorType -> et) <- fromException se
        , et == ResourceVanished || et == InvalidArgument = False
    | Just TimeoutThread <- fromException se = False
    | otherwise = True

-- | Printing an exception to standard error
--   if `defaultShouldDisplayException` returns `True`.
--
-- Since: 3.1.0
defaultOnException :: Maybe Request -> SomeException -> IO ()
defaultOnException _ e =
    when (defaultShouldDisplayException e)
        $ TIO.hPutStrLn stderr $ T.pack $ show e

-- | Sending 400 for bad requests. Sending 500 for internal server errors.
--
-- Since: 3.1.0
defaultOnExceptionResponse :: SomeException -> Response
defaultOnExceptionResponse e
  | Just (_ :: InvalidRequest) <- fromException e = responseLBS H.badRequest400  [(H.hContentType, "text/plain; charset=utf-8")] "Bad Request"
  | otherwise                                     = responseLBS H.internalServerError500 [(H.hContentType, "text/plain; charset=utf-8")] "Something went wrong"

-- | Exception handler for the debugging purpose.
--   500, text/plain, a showed exception.
--
-- Since: 2.0.3.2
exceptionResponseForDebug :: SomeException -> Response
exceptionResponseForDebug e =
    responseBuilder H.internalServerError500
                    [(H.hContentType, "text/plain; charset=utf-8")]
                    $ byteString . C8.pack $ "Exception: " ++ show e
