{-# LANGUAGE CPP #-}

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
module Network.Wai.Handler.Warp (
    -- * Run a Warp server
    run
  , runSettings
  , runSettingsSocket
  , runSettingsConnection
  , runSettingsConnectionMaker
    -- * Settings
  , Settings
  , defaultSettings
    -- ** Accessors
  , settingsPort
  , settingsHost
  , settingsOnException
  , settingsOnExceptionResponse
  , settingsOnOpen
  , settingsOnClose
  , settingsTimeout
  , settingsIntercept
  , settingsManager
  , settingsFdCacheDuration
  , settingsBeforeMainLoop
  , settingsNoParsePath
    -- ** Setters
  , setPort
  , setHost
  , setOnException
  , setOnExceptionResponse
  , setOnOpen
  , setOnClose
  , setTimeout
  , setIntercept
  , setManager
  , setFdCacheDuration
  , setBeforeMainLoop
  , setNoParsePath
    -- ** Debugging
  , exceptionResponseForDebug
    -- * Data types
  , HostPreference (..)
  , Port
  , InvalidRequest (..)
  , ConnSendFileOverride (..)
    -- * Connection
  , Connection (..)
  , socketConnection
    -- * Internal
    -- ** Version
  , warpVersion
    -- ** Data types
  , InternalInfo (..)
  , HeaderValue
  , IndexedHeader
  , requestMaxIndex
    -- ** Time out manager
  , module Network.Wai.Handler.Warp.Timeout
    -- ** File descriptor cache
  , module Network.Wai.Handler.Warp.FdCache
    -- ** Date
  , module Network.Wai.Handler.Warp.Date
    -- ** Request and response
  , recvRequest
  , sendResponse
  ) where

import Data.Conduit.Network (HostPreference(..))
import Network.Wai.Handler.Warp.Date
import Network.Wai.Handler.Warp.FdCache
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.Response
import Network.Wai.Handler.Warp.Run
import Network.Wai.Handler.Warp.Settings
import Network.Wai.Handler.Warp.Timeout
import Network.Wai.Handler.Warp.Types

setPort x y = y { settingsPort = x }
setHost x y = y { settingsHost = x }
setOnException x y = y { settingsOnException = x }
setOnExceptionResponse x y = y { settingsOnExceptionResponse = x }
setOnOpen x y = y { settingsOnOpen = x }
setOnClose x y = y { settingsOnClose = x }
setTimeout x y = y { settingsTimeout = x }
setIntercept x y = y { settingsIntercept = x }
setManager x y = y { settingsManager = x }
setFdCacheDuration x y = y { settingsFdCacheDuration = x }
setBeforeMainLoop x y = y { settingsBeforeMainLoop = x }
setNoParsePath x y = y { settingsNoParsePath = x }
