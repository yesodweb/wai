module Network.Wai.Middleware.AcceptOverride
    ( -- $howto
      acceptOverride
    ) where

import Control.Monad (join)
import Data.ByteString (ByteString)
import Network.Wai

-- $howto
-- This 'Middleware' provides a way for the request itself to
-- tell the server to override the \"Accept\" header by looking
-- for the \"_accept\" query parameter in the query string and
-- inserting or replacing the \"Accept\" header with that string.
--
-- For example:
--
-- @
-- ?_accept=SomeValue
-- @
--
-- This will result in \"Accept: SomeValue\" being set in the
-- request as a header, and all other previous \"Accept\" headers
-- will be removed from the request.

acceptOverride :: Middleware
acceptOverride app req =
    app req'
  where
    req' =
        case join $ lookup "_accept" $ queryString req of
            Nothing -> req
            Just a -> req { requestHeaders = changeVal "Accept" a $ requestHeaders req}

changeVal :: Eq a
          => a
          -> ByteString
          -> [(a, ByteString)]
          -> [(a, ByteString)]
changeVal key val old = (key, val)
                      : filter (\(k, _) -> k /= key) old
