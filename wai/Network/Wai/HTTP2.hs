{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Network.Wai.HTTP2
    ( Chunk(..)
    , HTTP2Application
    , PushPromise(..)
    , RespondFunc
    , Responder
    , Trailers
    , promiseHeaders
    , sendBuilder
    , sendFilePart
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

import           Data.ByteString.Builder (Builder)
import qualified Network.HTTP.Types as H

import Network.Wai.Internal (FilePart, Request)

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

-- TODO send -> write
-- | Given the write function of a stream body, write the given 'Builder'.
sendBuilder :: (Chunk -> IO ()) -> Builder -> IO ()
sendBuilder = (. BuilderChunk)

-- | Given the write function of a stream body, write part or all of the given
-- file.
sendFilePart :: (Chunk -> IO ()) -> FilePath -> Maybe FilePart -> IO ()
sendFilePart = (.: FileChunk)
  where f .: g = curry $ f . uncurry g

-- | The streaming body of a response.  Equivalent to
-- 'Network.Wai.StreamingBody' except that it can also write file ranges and
-- return a result of type @a@.
type Body a = (Chunk -> IO ()) -> IO () -> IO a

-- | A function given to an 'HTTP2Application' used to send the response.  This
-- is similar to the respond function in 'Network.Wai.Application', but the
-- status and headers are curried, and it passes on any result value from the
-- stream body.
type RespondFunc = forall a. H.Status -> H.ResponseHeaders -> Body a -> IO a

-- | A continuation-passing style function that provides a response by calling
-- the 'RespondFunc', then returns the request's 'Trailers'.
type Responder = RespondFunc -> IO Trailers

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
