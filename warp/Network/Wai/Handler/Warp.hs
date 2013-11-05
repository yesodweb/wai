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
  , settingsOnOpen
  , settingsOnClose
  , settingsTimeout
  , settingsIntercept
  , settingsManager
  , settingsFdCacheDuration
  , settingsBeforeMainLoop
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
    -- ** Request and response
  , recvRequest
  , sendResponse
#if TEST
  , takeHeaders
  , parseFirst
  , readInt
#endif
  ) where

import Data.Conduit.Network (HostPreference(..))
import Network.Wai.Handler.Warp.FdCache
import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.Response
import Network.Wai.Handler.Warp.Run
import Network.Wai.Handler.Warp.Settings
import Network.Wai.Handler.Warp.Timeout
import Network.Wai.Handler.Warp.Types
import Network.Wai.Handler.Warp.Header
