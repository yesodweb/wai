{-# LANGUAGE OverloadedStrings #-}
-- | Automatically produce responses to HEAD requests based on the underlying
-- applications GET response.
module Network.Wai.Middleware.Autohead (autohead) where

import Network.Wai
import Network.Wai.Internal
import Data.Monoid (mempty)

autohead :: Middleware
autohead app req
    | requestMethod req == "HEAD" = do
        res <- app req { requestMethod = "GET" }
        let go (ResponseFile s hs _ _) = ResponseBuilder s hs mempty
            go (ResponseBuilder s hs _) = ResponseBuilder s hs mempty
            go (ResponseSource s hs _) = ResponseBuilder s hs mempty
            go (ResponseRaw raw r) = ResponseRaw raw (go r)
        return (go res)
    | otherwise = app req

