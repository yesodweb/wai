{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.HTTP2.Types where

import qualified Data.ByteString as BS
import qualified Network.HTTP.Types as H
import Network.HTTP2.Frame
import qualified Network.HTTP2.Server as H2

import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

-- | Inspect ALPN only on TLS; TCP uses prior knowledge and QUIC uses HTTP/3.
-- Matching constructors keeps this dispatch total without partial TLS selectors.
isHTTP2 :: Transport -> Bool
isHTTP2 TCP = False
isHTTP2 TLS{tlsNegotiatedProtocol = protocol} = useHTTP2
  where
    useHTTP2 = case protocol of
        Nothing -> False
        Just proto -> "h2" `BS.isPrefixOf` proto
-- QUIC uses the HTTP/3 entry point, not HTTP/2 ALPN dispatch.
isHTTP2 QUIC{} = False

----------------------------------------------------------------

-- | HTTP/2 specific data.
--
--   Since: 3.2.7
data HTTP2Data = HTTP2Data
    { http2dataPushPromise :: [PushPromise]
    -- ^ Accessor for 'PushPromise' in 'HTTP2Data'.
    --
    --   Since: 3.2.7
    , http2dataTrailers :: H2.TrailersMaker
    -- ^ Accessor for 'H2.TrailersMaker' in 'HTTP2Data'.
    --
    --   Since: 3.2.8 but the type changed in 3.3.0
    }

-- | Default HTTP/2 specific data.
--
--   Since: 3.2.7
defaultHTTP2Data :: HTTP2Data
defaultHTTP2Data = HTTP2Data [] H2.defaultTrailersMaker

-- | HTTP/2 push promise or sever push.
--   This allows files only for backward-compatibility
--   while the HTTP/2 library supports other types.
--
--   Since: 3.2.7
data PushPromise = PushPromise
    { promisedPath :: ByteString
    -- ^ Accessor for a URL path in 'PushPromise'.
    --   E.g. \"\/style\/default.css\".
    --
    --   Since: 3.2.7
    , promisedFile :: FilePath
    -- ^ Accessor for 'FilePath' in 'PushPromise'.
    --   E.g. \"FILE_PATH/default.css\".
    --
    --   Since: 3.2.7
    , promisedResponseHeaders :: H.ResponseHeaders
    -- ^ Accessor for 'H.ResponseHeaders' in 'PushPromise'
    --   \"content-type\" must be specified.
    --   Default value: [].
    --
    --
    --   Since: 3.2.7
    , promisedWeight :: Weight
    -- ^ Accessor for 'Weight' in 'PushPromise'.
    --    Default value: 16.
    --
    --   Since: 3.2.7
    }
    deriving (Eq, Ord, Show)

-- | Default push promise.
--
--   Since: 3.2.7
defaultPushPromise :: PushPromise
defaultPushPromise = PushPromise "" "" [] 16
