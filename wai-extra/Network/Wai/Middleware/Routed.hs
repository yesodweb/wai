-- |
--
-- Since 3.0.9
module Network.Wai.Middleware.Routed (
    routedMiddleware,
    hostedMiddleware,
) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import Network.Wai

-- | Apply a middleware based on a test of pathInfo
--
-- example:
--
-- > let corsify = routedMiddleWare ("static" `elem`) addCorsHeaders
--
-- Since 3.0.9
routedMiddleware
    :: ([Text] -> Bool)
    -- ^ Only use middleware if this pathInfo test returns True
    -> Middleware
    -- ^ middleware to apply the path prefix guard to
    -> Middleware
    -- ^ modified middleware
routedMiddleware pathCheck middle app req
    | pathCheck (pathInfo req) = middle app req
    | otherwise = app req

-- | Only apply the middleware to certain hosts
--
-- Since 3.0.9
hostedMiddleware
    :: ByteString
    -- ^ Domain the middleware applies to
    -> Middleware
    -- ^ middleware to apply the path prefix guard to
    -> Middleware
    -- ^ modified middleware
hostedMiddleware domain middle app req
    | hasDomain domain req = middle app req
    | otherwise = app req

hasDomain :: ByteString -> Request -> Bool
hasDomain domain req = Just domain == requestHeaderHost req
