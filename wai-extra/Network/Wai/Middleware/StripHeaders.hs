-- This was written for one specific use case and then generalized.

-- The specific use case was a JSON API with a consumer that would choke on the
-- "Set-Cookie" response header. The solution was to test for the API's
-- `pathInfo` in the Request and if it matched, filter the response headers.

-- When using this, care should be taken not to strip out headers that are
-- required for correct operation of the client (eg Content-Type).

module Network.Wai.Middleware.StripHeaders
    ( stripHeader
    , stripHeaders
    , stripHeaderIf
    , stripHeadersIf
    ) where

import Network.Wai                       (Middleware, Request, modifyResponse, mapResponseHeaders, ifRequest)
import Network.Wai.Internal (Response)
import Data.ByteString                   (ByteString)

import qualified Data.CaseInsensitive as CI

stripHeader :: ByteString -> (Response -> Response)
stripHeader h = mapResponseHeaders (filter (\ hdr -> fst hdr /= CI.mk h))

stripHeaders :: [ByteString] -> (Response -> Response)
stripHeaders hs =
  let hnames = map CI.mk hs
  in mapResponseHeaders (filter (\ hdr -> fst hdr `notElem` hnames))

-- | If the request satisifes the provided predicate, strip headers matching
-- the provided header name.
--
-- Since 3.0.8
stripHeaderIf :: ByteString -> (Request -> Bool) -> Middleware
stripHeaderIf h rpred =
  ifRequest rpred (modifyResponse $ stripHeader h)

-- | If the request satisifes the provided predicate, strip all headers whose
-- header name is in the list of provided header names.
--
-- Since 3.0.8
stripHeadersIf :: [ByteString] -> (Request -> Bool) -> Middleware
stripHeadersIf hs rpred
  = ifRequest rpred (modifyResponse $ stripHeaders hs)
