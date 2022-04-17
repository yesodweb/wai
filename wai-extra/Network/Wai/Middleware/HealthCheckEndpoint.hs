---------------------------------------------------------
-- |
-- Module        : Network.Wai.Middleware.HealthCheckEndpoint
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Add empty endpoint (for Health check tests)
--
---------------------------------------------------------
module Network.Wai.Middleware.HealthCheckEndpoint
  ( healthCheck,
    voidEndpoint,
  )
where

import Data.ByteString (ByteString)
import Network.HTTP.Types (status200)
import Network.Wai

-- | Add empty endpoint (for Health check tests) called \"/_healthz\"
--
-- @since 3.1.9
healthCheck :: Middleware
healthCheck = voidEndpoint "/_healthz"

-- | Add empty endpoint
--
-- @since 3.1.9
voidEndpoint :: ByteString -> Middleware
voidEndpoint endpointPath router request respond =
  if rawPathInfo request == endpointPath
    then respond $ responseLBS status200 mempty "-"
    else router request respond
