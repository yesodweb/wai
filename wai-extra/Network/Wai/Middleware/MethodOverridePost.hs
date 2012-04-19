{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------
-- |
-- Module : Network.Wai.Middleware.MethodOverridePost
--
-- Changing of request-method via first post-parameter "_method".
--
-----------------------------------------------------------------
module Network.Wai.Middleware.MethodOverridePost
  ( methodOverridePost
  ) where

import Network.Wai
import Network.HTTP.Types           (parseQuery)
import Data.Monoid                  (mconcat)
import Data.Conduit.Lazy            (lazyConsume)
import Control.Monad.Trans.Resource (ResourceT)
import Data.Conduit.List            (sourceList)

-- | Override the request-method by setting the first POST key/value
-- to "_method" / <request-method>.
--
-- * Only accepts "application/x-www-form-urlencoded" POSTs.
methodOverridePost :: Middleware
methodOverridePost app req = case lookup "Content-Type" (requestHeaders req) of
  Just "application/x-www-form-urlencoded" -> setPost req >>= app
  _                                        -> app req

setPost :: Request -> ResourceT IO Request
setPost req = do
  body <- lazyConsume (requestBody req)
  case parseQuery (mconcat body) of (("_method", Just newmethod):_) -> return $ req {requestBody = sourceList body, requestMethod = newmethod}
                                    _                               -> return $ req {requestBody = sourceList body}
