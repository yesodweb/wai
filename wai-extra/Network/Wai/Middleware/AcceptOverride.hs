module Network.Wai.Middleware.AcceptOverride
    ( acceptOverride
    ) where

import Network.Wai
import Control.Monad (join)

import Network.Wai.Header (replaceHeader)

acceptOverride :: Middleware
acceptOverride app req =
    app req'
  where
    req' =
        case join $ lookup "_accept" $ queryString req of
            Nothing -> req
            Just a -> req {
                requestHeaders = replaceHeader "Accept" a $ requestHeaders req
              }
