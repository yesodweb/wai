-- This was written for one specific use case and then generalized.

-- The specific use case was a JSON API with a consumer that would choke on the
-- "Set-Cookie" response header. The solution was to test for the API's
-- `pathInfo` in the Request and if it matched, filter the response headers.

-- When using this, care should be taken not to strip out headers that are
-- required for correct operation of the client (eg Content-Type).

module Network.Wai.Middleware.StripHeaders
    ( stripHeader
    , stripHeaders
    ) where

import Network.Wai                       (Middleware, Request)
import Data.ByteString                   (ByteString)
import Network.Wai.Middleware.AddHeaders (mapResponseHeader)

import qualified Data.CaseInsensitive as CI


-- | If the request satisifes the provided predicate, strip headers matching
-- the provided header name.
--
-- Since 3.0.8

stripHeader :: ByteString -> (Request -> Bool) -> Middleware
stripHeader h rpred app req respond
    | rpred req = app req $ respond . mapResponseHeader (filter (\ hdr -> fst hdr /= CI.mk h))
    | otherwise = app req respond


-- | If the request satisifes the provided predicate, strip all headers whose
-- header name is in the list of provided header names.
--
-- Since 3.0.8

stripHeaders :: [ByteString] -> (Request -> Bool) -> Middleware
stripHeaders hs rpred app req respond
    | rpred req = do
        let hnames = map CI.mk hs
        app req $ respond . mapResponseHeader (filter (\ hdr -> fst hdr `notElem` hnames))
    | otherwise = app req respond
