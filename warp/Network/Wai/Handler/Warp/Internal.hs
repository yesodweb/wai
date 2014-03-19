{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Network.Wai.Handler.Warp.Internal
    ( Settings (..)
    , getOnOpen
    , getOnClose
    , getOnException
    ) where

import Network.Wai.Handler.Warp.Settings (Settings (..))
import Network.Socket (SockAddr)
import Network.Wai (Request)
import Control.Exception (SomeException)

getOnOpen :: Settings -> SockAddr -> IO Bool
getOnOpen = settingsOnOpen

getOnClose :: Settings -> SockAddr -> IO ()
getOnClose = settingsOnClose

getOnException :: Settings -> Maybe Request -> SomeException -> IO ()
getOnException = settingsOnException
