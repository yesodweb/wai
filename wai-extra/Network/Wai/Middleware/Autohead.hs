{-# LANGUAGE OverloadedStrings #-}
-- | Automatically produce responses to HEAD requests based on the underlying
-- applications GET response.
module Network.Wai.Middleware.Autohead (autohead) where

import Network.Wai
import Data.Monoid (mempty)

autohead :: Middleware
autohead app req sendResponse
    | requestMethod req == "HEAD" = app req { requestMethod = "GET" } $ \res -> do
        let (s, hs, _) = responseToStream res
        sendResponse $ responseBuilder s hs mempty
    | otherwise = app req sendResponse

