{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.MethodOverridePost ( methodOverridePost) where

import Network.Wai
import Data.Monoid                  (mconcat)
import Data.Conduit.Lazy            (lazyConsume)
import Network.HTTP.Types           (parseQuery)
import Control.Monad.Trans.Resource (ResourceT)

methodOverridePost :: Middleware
methodOverridePost app req = case lookup "Content-Type" (requestHeaders req) of
  Just "application/x-www-form-urlencoded" -> setPost req >>= app
  _                                        -> app req

setPost :: Request -> ResourceT IO Request
setPost req = do
  body <- lazyConsume (requestBody req)
  case parseQuery (mconcat body) of (("_method", Just newmethod):_) -> return $ req {requestMethod = newmethod}
                                    _                               -> return req
