{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------
-- | Module : Network.Wai.Middleware.MethodOverridePost
--
-- Changes the request-method via first post-parameter _method.
-----------------------------------------------------------------
module Network.Wai.Middleware.MethodOverridePost
  ( methodOverridePost
  ) where

import Network.Wai
import Network.HTTP.Types           (parseQuery)
import Data.Monoid                  (mconcat)

-- | Allows overriding of the HTTP request method via the _method post string parameter.
--
-- * Looks for the Content-Type requestHeader.
--
-- * If the header is set to application/x-www-form-urlencoded
-- and the first POST parameter is _method
-- then it changes the request-method to the value of that
-- parameter.
--
-- * This middleware only applies when the initial request method is POST.
--
methodOverridePost :: Middleware
methodOverridePost app req = case (requestMethod req, lookup "Content-Type" (requestHeaders req)) of
  ("POST", Just "application/x-www-form-urlencoded") -> setPost req >>= app
  _                                                  -> app req

setPost :: Request -> IO Request
setPost req = do
  body <- lazyConsume (requestBody req)
  case parseQuery (mconcat body) of
    (("_method", Just newmethod):_) -> return $ req {requestBody = sourceList body, requestMethod = newmethod}
    _                               -> return $ req {requestBody = sourceList body}
