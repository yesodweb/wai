module Network.Wai.Middleware.MethodOverride
    ( methodOverride
    ) where

import Network.Wai
import Control.Monad (join)

-- | Overriding of HTTP request method via `_method` query string parameter.
--
-- This middleware only applies when the initial request method is POST.
-- Allows submitting of normal HTML forms, without worries of semantic
-- mismatches with the HTTP spec.
methodOverride :: Middleware
methodOverride app req =
    app req'
  where
    req' =
        case (requestMethod req, join $ lookup "_method" $ queryString req) of
            ("POST", Just m) -> req { requestMethod = m }
            _ -> req
