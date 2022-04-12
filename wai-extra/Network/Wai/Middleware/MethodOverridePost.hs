{-# LANGUAGE CPP #-}
-----------------------------------------------------------------
-- | Module : Network.Wai.Middleware.MethodOverridePost
--
-- Changes the request-method via first post-parameter _method.
-----------------------------------------------------------------
module Network.Wai.Middleware.MethodOverridePost
  ( methodOverridePost
  ) where

import Data.ByteString.Lazy (toChunks)
import Data.IORef (atomicModifyIORef, newIORef)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mconcat, mempty)
#endif
import Network.HTTP.Types (hContentType, parseQuery)
import Network.Wai

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
methodOverridePost app req send =
    case (requestMethod req, lookup hContentType (requestHeaders req)) of
      ("POST", Just "application/x-www-form-urlencoded") -> setPost req >>= flip app send
      _                                                  -> app req send

setPost :: Request -> IO Request
setPost req = do
  body <- (mconcat . toChunks) `fmap` lazyRequestBody req
  ref <- newIORef body
  let rb = atomicModifyIORef ref $ \bs -> (mempty, bs)
  case parseQuery body of
    (("_method", Just newmethod):_) -> return $ req {requestBody = rb, requestMethod = newmethod}
    _                               -> return $ req {requestBody = rb}
