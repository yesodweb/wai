{-# LANGUAGE OverloadedStrings, RankNTypes #-}
{-# LANGUAGE CPP #-}
module Network.Wai.HTTP2
    (
    -- * Applications
      HTTP2Application
    , Responder
    , Body
    , Chunk(..)
    , BodyOf
    , PushFunc
    , Trailers
    -- * Server push
    , PushPromise(..)
    , promiseHeaders
    -- * Conveniences
    , SimpleBody
    , promoteApplication
    , responder
    , streamFilePart
    , streamBuilder
    , streamSimple
    ) where

#if __GLASGOW_HASKELL__ < 710
import           Data.Functor ((<$))
#endif
import           Blaze.ByteString.Builder (Builder)
import           Data.ByteString (ByteString)
import qualified Network.HTTP.Types as H

import           Network.Wai (Application)
import           Network.Wai.Internal
    ( FilePart
    , Request
    , Response(..)
    , ResponseReceived(..)
    )

-- | Headers sent after the end of a data stream, as defined by section 4.1.2 of
-- the HTTP\/1.1 spec (RFC 7230), and section 8.1 of the HTTP\/2 spec.
type Trailers = [H.Header]

-- | The synthesized request and headers of a pushed stream.
data PushPromise = PushPromise
    { promisedMethod :: H.Method
    , promisedPath :: ByteString
    , promisedAuthority :: ByteString
    , promisedScheme :: ByteString
    , promisedHeader :: H.RequestHeaders
    }

-- | The HTTP\/2-aware equivalent of 'Network.Wai.Application'.
type HTTP2Application = Request -> PushFunc -> Responder

-- | Part of a streaming response -- either a 'Builder' or a range of a file.
data Chunk = FileChunk FilePath (Maybe FilePart) | BuilderChunk Builder

-- | The streaming body of a response.  Equivalent to
-- 'Network.Wai.StreamingBody' except that it can also write file ranges and
-- return a result of type @a@.
type Body a = BodyOf Chunk a

-- | Generalization of 'Body' to arbitrary chunk types; this unifies with
-- 'Network.Wai.StreamingBody' with @c ~ Builder@ and @a ~ ()@.
type BodyOf c a = (c -> IO ()) -> IO () -> IO a

-- | The result of an 'HTTP2Application'; or, alternately, an application
-- that's independent of the request.  This is a continuation-passing style
-- function that first provides a response by calling the given respond
-- function, then returns the request's 'Trailers'.
--
-- The respond function is similar to the one in 'Network.Wai.Application', but
-- it only takes a streaming body, the status and headers are curried, and it
-- passes on any result value from the stream body.
type Responder = (forall a. H.Status -> H.ResponseHeaders -> Body a -> IO a)
              -> IO Trailers

-- | A function given to an 'HTTP2Application' to initiate a server-pushed
-- stream.  Its argument is the same as the result of an 'HTTP2Application', so
-- you can either implement the response inline, or call your own application
-- to create the response.
type PushFunc = PushPromise -> Responder -> IO Bool

-- | Create the 'H.RequestHeaders' corresponding to the given 'PushPromise'.
promiseHeaders :: PushPromise -> H.RequestHeaders
promiseHeaders p =
  [ (":method", promisedMethod p)
  , (":path", promisedPath p)
  , (":authority", promisedAuthority p)
  , (":scheme", promisedScheme p)
  ] ++ promisedHeader p

-- | Create a response body consisting of a single range of a file.
streamFilePart :: FilePath -> Maybe FilePart -> Body ()
streamFilePart path part write _ = write $ FileChunk path part

-- | Create a response body consisting of a single builder.
streamBuilder :: Builder -> Body ()
streamBuilder builder write _ = write $ BuilderChunk builder

-- | Equivalent to 'Body' but only streaming 'Builder's.
--
-- @'Network.Wai.StreamingBody' ~ SimpleBody ()@.
type SimpleBody a = BodyOf Builder a

-- | Create a response body of a stream of 'Builder's.
streamSimple :: SimpleBody a -> Body a
streamSimple body write flush = body (write . BuilderChunk) flush

-- | Use a normal WAI 'Response' to send the response.  Useful if you're
-- sharing code between HTTP\/2 applications and HTTP\/1 applications.
responder :: Response -> Responder
responder response respond = case response of
    (ResponseFile s h path part) -> [] <$ respond s h (streamFilePart path part)
    (ResponseBuilder s h b)      -> [] <$ respond s h (streamBuilder b)
    (ResponseStream s h body)    -> [] <$ respond s h (streamSimple body)
    (ResponseRaw _ fallback)     -> responder fallback respond

-- | Promote a normal WAI 'Application' to an 'HTTP2Application' by ignoring
-- the HTTP/2-specific features.
promoteApplication :: Application -> HTTP2Application
promoteApplication app req _ respond = [] <$ app req respond'
  where respond' r = ResponseReceived <$ responder r respond
