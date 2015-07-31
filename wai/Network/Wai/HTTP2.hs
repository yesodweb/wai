{-# LANGUAGE RankNTypes #-}
module Network.Wai.HTTP2
    ( Http2Application
    , Response
    , Trailers
    , responseStatus
    , responseStream
    , responseHeaders
    ) where

import           Blaze.ByteString.Builder     (Builder)
import qualified Network.HTTP.Types as H

import Network.Wai.Internal (Request)

-- | Headers sent after the end of a data stream, as defined by section 4.1.2 of
-- the HTTP\/1.1 spec (RFC 7230), and section 8.1 of the HTTP\/2 spec.
type Trailers = H.ResponseHeaders

-- | The HTTP\/2-aware equivalent of 'Network.Wai.Application'.
type Http2Application = Request -> (forall a. Response a -> IO a) -> IO Trailers

type StreamingBody a = (Builder -> IO ()) -> IO () -> IO a

type Response a = (H.Status, H.ResponseHeaders, StreamingBody a)

responseStatus :: Response a -> H.Status
responseStatus (s, _, _) = s

responseHeaders :: Response a -> H.ResponseHeaders
responseHeaders (_, h, _) = h

responseStream :: H.Status -> H.ResponseHeaders -> StreamingBody a -> Response a
responseStream = (,,)
