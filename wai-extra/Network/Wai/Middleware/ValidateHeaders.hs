module Network.Wai.Middleware.ValidateHeaders (
    -- * Middleware
    validateHeadersMiddleware
) where

import Network.Wai (Middleware)

-- | TODO
--
-- @since 3.1.15
validateHeadersMiddleware :: Middleware
validateHeadersMiddleware app req respond =
    app req respond
