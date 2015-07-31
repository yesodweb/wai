module Network.Wai.HTTP2
    ( Http2Application
    , Response(..)
    , Trailers
    , responseStatus
    , responseHeaders
    ) where

import           Blaze.ByteString.Builder     (Builder)
import qualified Data.ByteString              as B
import qualified Network.HTTP.Types as H

import Network.Wai.Internal (Request, FilePart)

-- | Headers sent after the end of a data stream, as defined by section 4.1.2 of
-- the HTTP\/1.1 spec (RFC 7230), and section 8.1 of the HTTP\/2 spec.
type Trailers = H.ResponseHeaders

-- | The HTTP\/2-aware equivalent of 'Network.Wai.Application'.
type Http2Application = Request -> (Response -> IO ()) -> IO Trailers

type StreamingBody = (Builder -> IO ()) -> IO () -> IO ()

data Response
    = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
    | ResponseBuilder H.Status H.ResponseHeaders Builder
    | ResponseStream H.Status H.ResponseHeaders StreamingBody
    | ResponseRaw (IO B.ByteString -> (B.ByteString -> IO ()) -> IO ()) Response

responseStatus :: Response -> H.Status
responseStatus (ResponseFile s _ _ _) = s
responseStatus (ResponseBuilder s _ _) = s
responseStatus (ResponseStream s _ _) = s
responseStatus (ResponseRaw _ res) = responseStatus res

responseHeaders :: Response -> H.ResponseHeaders
responseHeaders _ = []

