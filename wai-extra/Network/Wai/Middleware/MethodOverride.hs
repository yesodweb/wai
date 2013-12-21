{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.MethodOverride
    ( methodOverride
    ) where

import Network.Wai
import Control.Monad (join)

-- | Allows overriding of the HTTP request method via the _method query string
-- parameter.
--
-- This middleware only applies when the initial request method is POST. This
-- allow submitting of normal HTML forms, without worries of semantics
-- mismatches in the HTTP spec.
methodOverride :: Middleware
methodOverride app req =
    app req'
  where
    req' =
        case (requestMethod req, join $ lookup "_method" $ queryString req) of
            ("POST", Just m) -> req { requestMethod = m }
            _ -> req
