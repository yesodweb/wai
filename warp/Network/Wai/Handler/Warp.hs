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
    -- * Settings
  , Settings
  , defaultSettings
  , settingsPort
  , settingsHost
  , settingsOnException
  , settingsOnOpen
  , settingsOnClose
  , settingsTimeout
  , settingsIntercept
  , settingsManager
    -- ** Data types
  , HostPreference (..)
    -- * Connection
  , Connection (..)
  , runSettingsConnection
    -- * Datatypes
  , Port
  , InvalidRequest (..)
    -- * Internal
  , Manager
  , withManager
  , parseRequest
  , sendResponse
  , registerKillThread
  , pause
  , resume
  , cancel
  , register
  , initialize
  , socketConnection
#if TEST
  , takeHeaders
  , parseFirst
  , readInt
#endif
  ) where

import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.Response
import Network.Wai.Handler.Warp.Run
import Network.Wai.Handler.Warp.Settings
import Network.Wai.Handler.Warp.Types
import Network.Wai.Handler.Warp.Timeout
import Data.Conduit.Network (HostPreference(..))
