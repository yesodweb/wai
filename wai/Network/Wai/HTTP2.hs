{-# LANGUAGE OverloadedStrings, RankNTypes #-}
module Network.Wai.HTTP2
    ( HTTP2Application
    , PushPromise(..)
    , RespondFunc
    , Responder
    , Trailers
    , promiseHeaders
    ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L

import           Data.ByteString.Builder (Builder)
import qualified Network.HTTP.Types as H

import Network.Wai.Internal (Request)

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

type Body a = (Builder -> IO ()) -> IO () -> IO a

type RespondFunc = forall a. H.Status -> H.ResponseHeaders -> Body a -> IO a

type Responder = RespondFunc -> IO Trailers

type PushFunc = PushPromise -> Responder -> IO Bool

promiseHeaders :: PushPromise -> H.RequestHeaders
promiseHeaders p =
  [ (":method", promisedMethod p)
  , (":path", promisedPath p)
  , (":authority", promisedAuthority p)
  , (":scheme", promisedScheme p)
  ] ++ promisedHeader p
