module Network.Wai.Middleware.ForceDomain where

import Data.ByteString (ByteString)
import Data.Monoid ((<>), mempty)
import Network.HTTP.Types (hLocation, methodGet, status301, status307)
import Prelude

import Network.Wai
import Network.Wai.Request

-- | Force a domain by redirecting. 
-- The `checkDomain` function takes the current domain and checks whether it is correct. 
-- It should return `Nothing` if the domain is correct, or `Just "domain.com"` if it is incorrect. 
forceDomain :: (ByteString -> Maybe ByteString) -> Middleware
forceDomain checkDomain app req sendResponse = 
    case requestHeaderHost req >>= checkDomain of
        Nothing ->
            app req sendResponse
        Just domain ->
            sendResponse $ redirectResponse domain

    where
        -- From: Network.Wai.Middleware.ForceSSL
        redirectResponse domain = 
            responseBuilder status [(hLocation, location domain)] mempty

        location h = 
            let p = if appearsSecure req then "https://" else "http://" in
            p <> h <> rawPathInfo req <> rawQueryString req

        status
            | requestMethod req == methodGet = status301
            | otherwise = status307
