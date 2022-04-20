---------------------------------------------------------
-- |
-- Module        : Network.Wai.Middleware.Select
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Dynamically choose between Middlewares
--
-- It's useful when you want some 'Middleware's applied selectively.
--
-- Example: do not log health check calls:
--
-- > import Network.Wai
-- > import Network.Wai.Middleware.HealthCheckEndpoint
-- > import Network.Wai.Middleware.RequestLogger
-- >
-- > app' :: Application
-- > app' =
-- >   selectMiddleware (selectMiddlewareExceptRawPathInfo "/_healthz" logStdout)
-- >     $ healthCheck app
--
-- @since 3.1.10
--
---------------------------------------------------------
module Network.Wai.Middleware.Select
  ( -- * Middleware selection
    MiddlewareSelection (..),
    selectMiddleware,

    -- * Helpers
    selectMiddlewareOn,
    selectMiddlewareOnRawPathInfo,
    selectMiddlewareExceptRawPathInfo,
    passthroughMiddleware,
  )
where

import Control.Applicative ((<|>))
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Network.Wai

--------------------------------------------------
-- * Middleware selection
--------------------------------------------------

-- | Relevant Middleware for a given 'Request'.
newtype MiddlewareSelection = MiddlewareSelection
  { applySelectedMiddleware :: Request -> Maybe Middleware
  }

instance Semigroup MiddlewareSelection where
  MiddlewareSelection f <> MiddlewareSelection g =
    MiddlewareSelection $ \req -> f req <|> g req

instance Monoid MiddlewareSelection where
  mempty = MiddlewareSelection $ const Nothing

-- | Create the 'Middleware' dynamically applying 'MiddlewareSelection'.
selectMiddleware :: MiddlewareSelection -> Middleware
selectMiddleware selection app request respond =
  mw app request respond
  where
    mw :: Middleware
    mw = fromMaybe passthroughMiddleware (applySelectedMiddleware selection request)

--------------------------------------------------
-- * Helpers
--------------------------------------------------

passthroughMiddleware :: Middleware
passthroughMiddleware = id

-- | Use the 'Middleware' when the predicate holds.
selectMiddlewareOn :: (Request -> Bool) -> Middleware -> MiddlewareSelection
selectMiddlewareOn doesApply mw = MiddlewareSelection $ \request ->
  if doesApply request
    then Just mw
    else Nothing

-- | Use the `Middleware` for the given 'rawPathInfo'.
selectMiddlewareOnRawPathInfo :: ByteString -> Middleware -> MiddlewareSelection
selectMiddlewareOnRawPathInfo path = selectMiddlewareOn ((== path) . rawPathInfo)

-- | Use the `Middleware` for all 'rawPathInfo' except then given one.
selectMiddlewareExceptRawPathInfo :: ByteString -> Middleware -> MiddlewareSelection
selectMiddlewareExceptRawPathInfo path = selectMiddlewareOn ((/= path) . rawPathInfo)
