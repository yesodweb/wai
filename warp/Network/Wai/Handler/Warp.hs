{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

---------------------------------------------------------
--
-- Module        : Network.Wai.Handler.Warp
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Stable
-- Portability   : portable
--
-- A fast, light-weight HTTP server handler for WAI.
--
---------------------------------------------------------

-- | A fast, light-weight HTTP server handler for WAI.
--
-- HTTP\/1.0, HTTP\/1.1 and HTTP\/2 are supported. For HTTP\/2,
-- Warp supports direct and ALPN (in TLS) but not upgrade.
--
-- Note on slowloris timeouts: to prevent slowloris attacks, timeouts are used
-- at various points in request receiving and response sending. One interesting
-- corner case is partial request body consumption; in that case, Warp's
-- timeout handling is still in effect, and the timeout will not be triggered
-- again. Therefore, it is recommended that once you start consuming the
-- request body, you either:
--
-- * consume the entire body promptly
--
-- * call the 'pauseTimeout' function
--
-- For more information, see <https://github.com/yesodweb/wai/issues/351>.
--
--
module Network.Wai.Handler.Warp (
    -- * Run a Warp server
    -- | All of these automatically serve the same 'Application' over HTTP\/1,
    -- HTTP\/1.1, and HTTP\/2.
    run
  , runEnv
  , runSettings
  , runSettingsSocket
    -- * Settings
  , Settings
  , defaultSettings
    -- ** Setters
  , setPort
  , setHost
  , setOnException
  , setOnExceptionResponse
  , setOnOpen
  , setOnClose
  , setTimeout
  , setManager
  , setFdCacheDuration
  , setFileInfoCacheDuration
  , setBeforeMainLoop
  , setNoParsePath
  , setInstallShutdownHandler
  , setServerName
  , setMaximumBodyFlush
  , setFork
  , setProxyProtocolNone
  , setProxyProtocolRequired
  , setProxyProtocolOptional
  , setSlowlorisSize
  , setHTTP2Disabled
  , setLogger
  , setServerPushLogger
  , setGracefulShutdownTimeout
    -- ** Getters
  , getPort
  , getHost
  , getOnOpen
  , getOnClose
  , getOnException
  , getGracefulShutdownTimeout
    -- ** Exception handler
  , defaultOnException
  , defaultShouldDisplayException
    -- ** Exception response handler
  , defaultOnExceptionResponse
  , exceptionResponseForDebug
    -- * Data types
  , HostPreference
  , Port
  , InvalidRequest (..)
    -- * Utilities
  , pauseTimeout
  , FileInfo(..)
  , getFileInfo
  , withApplication
  , withApplicationSettings
  , testWithApplication
  , testWithApplicationSettings
  , openFreePort
    -- * Version
  , warpVersion
    -- * HTTP/2
    -- ** HTTP2 data
  , HTTP2Data
  , http2dataPushPromise
  , http2dataTrailers
  , defaultHTTP2Data
  , getHTTP2Data
  , setHTTP2Data
  , modifyHTTP2Data
    -- ** Push promise
  , PushPromise
  , promisedPath
  , promisedFile
  , promisedResponseHeaders
  , promisedWeight
  , defaultPushPromise
  ) where

import Control.Exception (SomeException, throwIO)
import Data.Streaming.Network (HostPreference)
import qualified Data.Vault.Lazy as Vault
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)
import Network.Wai (Request, Response, vault)

import Network.Wai.Handler.Warp.FileInfoCache
import Network.Wai.Handler.Warp.HTTP2.Request (getHTTP2Data, setHTTP2Data, modifyHTTP2Data)
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.Response (warpVersion)
import Network.Wai.Handler.Warp.Run
import Network.Wai.Handler.Warp.Settings
import Network.Wai.Handler.Warp.Timeout
import Network.Wai.Handler.Warp.Types hiding (getFileInfo)
import Network.Wai.Handler.Warp.WithApplication

-- | Port to listen on. Default value: 3000
--
-- Since 2.1.0
setPort :: Port -> Settings -> Settings
setPort x y = y { settingsPort = x }

-- | Interface to bind to. Default value: HostIPv4
--
-- Since 2.1.0
setHost :: HostPreference -> Settings -> Settings
setHost x y = y { settingsHost = x }

-- | What to do with exceptions thrown by either the application or server.
-- Default: 'defaultOnException'
--
-- Since 2.1.0
setOnException :: (Maybe Request -> SomeException -> IO ()) -> Settings -> Settings
setOnException x y = y { settingsOnException = x }

-- | A function to create a `Response` when an exception occurs.
-- Default: 'defaultOnExceptionResponse'
--
-- Note that an application can handle its own exceptions without interfering with Warp:
--
-- > myApp :: Application
-- > myApp request respond = innerApp `catch` onError
-- >   where
-- >     onError = respond . response500 request
-- >
-- > response500 :: Request -> SomeException -> Response
-- > response500 req someEx = responseLBS status500 -- ...
--
-- Since 2.1.0
setOnExceptionResponse :: (SomeException -> Response) -> Settings -> Settings
setOnExceptionResponse x y = y { settingsOnExceptionResponse = x }

-- | What to do when a connection is opened. When 'False' is returned, the
-- connection is closed immediately. Otherwise, the connection is going on.
-- Default: always returns 'True'.
--
-- Since 2.1.0
setOnOpen :: (SockAddr -> IO Bool) -> Settings -> Settings
setOnOpen x y = y { settingsOnOpen = x }

-- | What to do when a connection is closed. Default: do nothing.
--
-- Since 2.1.0
setOnClose :: (SockAddr -> IO ()) -> Settings -> Settings
setOnClose x y = y { settingsOnClose = x }

-- | Timeout value in seconds. Default value: 30
--
-- Since 2.1.0
setTimeout :: Int -> Settings -> Settings
setTimeout x y = y { settingsTimeout = x }

-- | Use an existing timeout manager instead of spawning a new one. If used,
-- 'settingsTimeout' is ignored.
--
-- Since 2.1.0
setManager :: Manager -> Settings -> Settings
setManager x y = y { settingsManager = Just x }

-- | Cache duration time of file descriptors in seconds. 0 means that the cache mechanism is not used.
--
-- The FD cache is an optimization that is useful for servers dealing with
-- static files. However, if files are being modified, it can cause incorrect
-- results in some cases. Therefore, we disable it by default. If you know that
-- your files will be static or you prefer performance to file consistency,
-- it's recommended to turn this on; a reasonable value for those cases is 10.
-- Enabling this cache results in drastic performance improvement for file
-- transfers.
--
-- Default value: 0, was previously 10
--
-- Since 3.0.13
setFdCacheDuration :: Int -> Settings -> Settings
setFdCacheDuration x y = y { settingsFdCacheDuration = x }

-- | Cache duration time of file information in seconds. 0 means that the cache mechanism is not used.
--
-- The file information cache is an optimization that is useful for servers dealing with
-- static files. However, if files are being modified, it can cause incorrect
-- results in some cases. Therefore, we disable it by default. If you know that
-- your files will be static or you prefer performance to file consistency,
-- it's recommended to turn this on; a reasonable value for those cases is 10.
-- Enabling this cache results in drastic performance improvement for file
-- transfers.
--
-- Default value: 0
setFileInfoCacheDuration :: Int -> Settings -> Settings
setFileInfoCacheDuration x y = y { settingsFileInfoCacheDuration = x }

-- | Code to run after the listening socket is ready but before entering
-- the main event loop. Useful for signaling to tests that they can start
-- running, or to drop permissions after binding to a restricted port.
--
-- Default: do nothing.
--
-- Since 2.1.0
setBeforeMainLoop :: IO () -> Settings -> Settings
setBeforeMainLoop x y = y { settingsBeforeMainLoop = x }

-- | Perform no parsing on the rawPathInfo.
--
-- This is useful for writing HTTP proxies.
--
-- Default: False
--
-- Since 2.1.0
setNoParsePath :: Bool -> Settings -> Settings
setNoParsePath x y = y { settingsNoParsePath = x }

-- | Get the listening port.
--
-- Since 2.1.1
getPort :: Settings -> Port
getPort = settingsPort

-- | Get the interface to bind to.
--
-- Since 2.1.1
getHost :: Settings -> HostPreference
getHost = settingsHost

-- | Get the action on opening connection.
getOnOpen :: Settings -> SockAddr -> IO Bool
getOnOpen = settingsOnOpen

-- | Get the action on closeing connection.
getOnClose :: Settings -> SockAddr -> IO ()
getOnClose = settingsOnClose

-- | Get the exception handler.
getOnException :: Settings -> Maybe Request -> SomeException -> IO ()
getOnException = settingsOnException

-- | Get the graceful shutdown timeout
--
-- Since 3.2.8
getGracefulShutdownTimeout :: Settings -> Maybe Int
getGracefulShutdownTimeout = settingsGracefulShutdownTimeout

-- | A code to install shutdown handler.
--
-- For instance, this code should set up a UNIX signal
-- handler. The handler should call the first argument,
-- which closes the listen socket, at shutdown.
--
-- Example usage:
--
-- @
-- settings :: IO () -> 'Settings'
-- settings shutdownAction = 'setInstallShutdownHandler' shutdownHandler 'defaultSettings'
--   __where__
--     shutdownHandler closeSocket =
--       void $ 'System.Posix.Signals.installHandler' 'System.Posix.Signals.sigTERM' ('System.Posix.Signals.Catch' $ shutdownAction >> closeSocket) 'Nothing'
-- @
--
-- Default: does not install any code.
--
-- Since 3.0.1
setInstallShutdownHandler :: (IO () -> IO ()) -> Settings -> Settings
setInstallShutdownHandler x y = y { settingsInstallShutdownHandler = x }

-- | Default server name to be sent as the \"Server:\" header
--   if an application does not set one.
--   If an empty string is set, the \"Server:\" header is not sent.
--   This is true even if an application set one.
--
-- Since 3.0.2
setServerName :: ByteString -> Settings -> Settings
setServerName x y = y { settingsServerName = x }

-- | The maximum number of bytes to flush from an unconsumed request body.
--
-- By default, Warp does not flush the request body so that, if a large body is
-- present, the connection is simply terminated instead of wasting time and
-- bandwidth on transmitting it. However, some clients do not deal with that
-- situation well. You can either change this setting to @Nothing@ to flush the
-- entire body in all cases, or in your application ensure that you always
-- consume the entire request body.
--
-- Default: 8192 bytes.
--
-- Since 3.0.3
setMaximumBodyFlush :: Maybe Int -> Settings -> Settings
setMaximumBodyFlush x y
    | Just x' <- x, x' < 0 = error "setMaximumBodyFlush: must be positive"
    | otherwise = y { settingsMaximumBodyFlush = x }

-- | Code to fork a new thread to accept a connection.
--
-- This may be useful if you need OS bound threads, or if
-- you wish to develop an alternative threading model.
--
-- Default: void . forkIOWithUnmask
--
-- Since 3.0.4
setFork :: (((forall a. IO a -> IO a) -> IO ()) -> IO ()) -> Settings -> Settings
setFork fork' s = s { settingsFork = fork' }

-- | Do not use the PROXY protocol.
--
-- Since 3.0.5
setProxyProtocolNone :: Settings -> Settings
setProxyProtocolNone y = y { settingsProxyProtocol = ProxyProtocolNone }

-- | Require PROXY header.
--
-- This is for cases where a "dumb" TCP/SSL proxy is being used, which cannot
-- add an @X-Forwarded-For@ HTTP header field but has enabled support for the
-- PROXY protocol.
--
-- See <http://www.haproxy.org/download/1.5/doc/proxy-protocol.txt> and
-- <http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#proxy-protocol>.
--
-- Only the human-readable header format (version 1) is supported. The binary
-- header format (version 2) is /not/ supported.
--
-- Since 3.0.5
setProxyProtocolRequired :: Settings -> Settings
setProxyProtocolRequired y = y { settingsProxyProtocol = ProxyProtocolRequired }

-- | Use the PROXY header if it exists, but also accept
-- connections without the header.  See 'setProxyProtocolRequired'.
--
-- WARNING: This is contrary to the PROXY protocol specification and
-- using it can indicate a security problem with your
-- architecture if the web server is directly accessable
-- to the public, since it would allow easy IP address
-- spoofing.  However, it can be useful in some cases,
-- such as if a load balancer health check uses regular
-- HTTP without the PROXY header, but proxied
-- connections /do/ include the PROXY header.
--
-- Since 3.0.5
setProxyProtocolOptional :: Settings -> Settings
setProxyProtocolOptional y = y { settingsProxyProtocol = ProxyProtocolOptional }

-- | Size in bytes read to prevent Slowloris attacks. Default value: 2048
--
-- Since 3.1.2
setSlowlorisSize :: Int -> Settings -> Settings
setSlowlorisSize x y = y { settingsSlowlorisSize = x }

-- | Disable HTTP2.
--
-- Since 3.1.7
setHTTP2Disabled :: Settings -> Settings
setHTTP2Disabled y = y { settingsHTTP2Enabled = False }

-- | Setting a log function.
--
-- Since 3.X.X
setLogger :: (Request -> H.Status -> Maybe Integer -> IO ()) -- ^ request, status, maybe file-size
          -> Settings
          -> Settings
setLogger lgr y = y { settingsLogger = lgr }

-- | Setting a log function for HTTP/2 server push.
--
--   Since: 3.2.7
setServerPushLogger :: (Request -> ByteString -> Integer -> IO ()) -- ^ request, path, file-size
                    -> Settings
                    -> Settings
setServerPushLogger lgr y = y { settingsServerPushLogger = lgr }

-- | Set the graceful shutdown timeout. A timeout of `Nothing' will
-- wait indefinitely, and a number, if provided, will be treated as seconds
-- to wait for requests to finish, before shutting down the server entirely.
--
-- Since 3.2.8
setGracefulShutdownTimeout :: Maybe Int
                           -> Settings -> Settings
setGracefulShutdownTimeout time y = y { settingsGracefulShutdownTimeout = time }

-- | Explicitly pause the slowloris timeout.
--
-- This is useful for cases where you partially consume a request body. For
-- more information, see <https://github.com/yesodweb/wai/issues/351>
--
-- Since 3.0.10
pauseTimeout :: Request -> IO ()
pauseTimeout = fromMaybe (return ()) . Vault.lookup pauseTimeoutKey . vault

-- | Getting file information of the target file.
--
--   This function first uses a stat(2) or similar system call
--   to obtain information of the target file, then registers
--   it into the internal cache.
--   From the next time, the information is obtained
--   from the cache. This reduces the overhead to call the system call.
--   The internal cache is refreshed every duration specified by
--   'setFileInfoCacheDuration'.
--
--   This function throws an 'IO' exception if the information is not
--   available. For instance, the target file does not exist.
--   If this function is used an a Request generated by a WAI
--   backend besides Warp, it also throws an 'IO' exception.
--
-- Since 3.1.10
getFileInfo :: Request -> FilePath -> IO FileInfo
getFileInfo = fromMaybe (\_ -> throwIO (userError "getFileInfo")) . Vault.lookup getFileInfoKey . vault
