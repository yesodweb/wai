{-# LANGUAGE CPP #-}
-- | Redirect non-SSL requests to https
--
-- Since 3.0.7
module  Network.Wai.Middleware.ForceSSL
    ( forceSSL
    ) where

import Network.Wai
import Network.Wai.Request

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
import Data.Monoid (mempty)
#endif

import Data.Monoid ((<>))
import Network.HTTP.Types (hLocation, methodGet, status301, status307)

-- | For requests that don't appear secure, redirect to https
--
-- Since 3.0.7
forceSSL :: Middleware
forceSSL app req sendResponse =
    case (appearsSecure req, redirectResponse req) of
        (False, Just resp) -> sendResponse resp
        _                  -> app req sendResponse

redirectResponse :: Request -> Maybe Response
redirectResponse req = do
  host <- requestHeaderHost req
  return $ responseBuilder status [(hLocation, location host)] mempty
  where
    location h = "https://" <> h <> rawPathInfo req <> rawQueryString req
    status
        | requestMethod req == methodGet = status301
        | otherwise = status307
