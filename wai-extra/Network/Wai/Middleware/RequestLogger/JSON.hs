{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Middleware.RequestLogger.JSON
  ( formatAsJSON
  , formatAsJSONWithHeaders
  ) where

import qualified Data.ByteString.Builder as BB (toLazyByteString)
import Data.ByteString.Lazy (toStrict)
import Data.Aeson
import Data.CaseInsensitive (original)
import Data.Monoid ((<>))
import qualified Data.ByteString.Char8 as S8
import Data.IP
import qualified Data.Text as T
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (NominalDiffTime)
import Data.Word (Word32)
import Network.HTTP.Types as H
import Network.Socket (SockAddr (..), PortNumber)
import Network.Wai
import Network.Wai.Middleware.RequestLogger
import System.Log.FastLogger (toLogStr)
import Text.Printf (printf)

formatAsJSON :: OutputFormatterWithDetails
formatAsJSON date req status responseSize duration reqBody response =
  toLogStr (encode $
    object
      [ "request"  .= requestToJSON duration req reqBody
      , "response" .=
      object
        [ "status" .= statusCode status
        , "size"   .= responseSize
        , "body"   .=
          if statusCode status >= 400
            then Just . decodeUtf8With lenientDecode . toStrict . BB.toLazyByteString $ response
            else Nothing
        ]
      , "time"     .= decodeUtf8With lenientDecode date
      ]) <> "\n"

-- | Same as @formatAsJSON@ but with response headers included
--
-- This is useful for passing arbitrary data from your application out to the
-- WAI layer for it to be logged, but you may need to be careful to
-- subsequently redact any headers which may contain sensitive data.
--
-- @since 3.0.27
formatAsJSONWithHeaders :: OutputFormatterWithDetailsAndHeaders
formatAsJSONWithHeaders date req status resSize duration reqBody res resHeaders =
  toLogStr (encode $
    object
      [ "request"  .= requestToJSON duration req reqBody
      , "response" .= object
        [ "status" .= statusCode status
        , "size"   .= resSize
        , "headers" .= responseHeadersToJSON resHeaders
        , "body"   .=
          if statusCode status >= 400
            then Just . decodeUtf8With lenientDecode . toStrict . BB.toLazyByteString $ res
            else Nothing
        ]
      , "time"     .= decodeUtf8With lenientDecode date
      ]) <> "\n"

word32ToHostAddress :: Word32 -> Text
word32ToHostAddress = T.intercalate "." . map (T.pack . show) . fromIPv4 . fromHostAddress

readAsDouble :: String -> Double
readAsDouble = read

requestToJSON :: NominalDiffTime -> Request -> [S8.ByteString] -> Value
requestToJSON duration req reqBody =
  object
    [ "method" .= decodeUtf8With lenientDecode (requestMethod req)
    , "path" .= decodeUtf8With lenientDecode (rawPathInfo req)
    , "queryString" .= map queryItemToJSON (queryString req)
    , "durationMs" .= (readAsDouble . printf "%.2f" . rationalToDouble $ toRational duration * 1000)
    , "size" .= requestBodyLengthToJSON (requestBodyLength req)
    , "body" .= decodeUtf8With lenientDecode (S8.concat reqBody)
    , "remoteHost" .= sockToJSON (remoteHost req)
    , "httpVersion" .= httpVersionToJSON (httpVersion req)
    , "headers" .= requestHeadersToJSON (requestHeaders req)
    ]
  where
    rationalToDouble :: Rational -> Double
    rationalToDouble = fromRational

sockToJSON :: SockAddr -> Value
sockToJSON (SockAddrInet pn ha) =
  object
    [ "port" .= portToJSON pn
    , "hostAddress" .= word32ToHostAddress ha
    ]
sockToJSON (SockAddrInet6 pn _ ha _) =
  object
    [ "port" .= portToJSON pn
    , "hostAddress" .= ha
    ]
sockToJSON (SockAddrUnix sock) =
  object [ "unix" .= sock ]
#if !MIN_VERSION_network(3,0,0)
sockToJSON (SockAddrCan i) =
  object [ "can" .= i ]
#endif

queryItemToJSON :: QueryItem -> Value
queryItemToJSON (name, mValue) = toJSON (decodeUtf8With lenientDecode name, fmap (decodeUtf8With lenientDecode) mValue)

requestHeadersToJSON :: RequestHeaders -> Value
requestHeadersToJSON = toJSON . map hToJ where
  -- Redact cookies
  hToJ ("Cookie", _) = toJSON ("Cookie" :: Text, "-RDCT-" :: Text)
  -- Redact tokens
  hToJ ("Authorization", _) = toJSON ("Authorization" :: Text, "-RDCT-" :: Text)
  hToJ hd = headerToJSON hd

responseHeadersToJSON :: [Header] -> Value
responseHeadersToJSON = toJSON . map hToJ where
  -- Redact cookies
  hToJ ("Set-Cookie", _) = toJSON ("Set-Cookie" :: Text, "-RDCT-" :: Text)
  hToJ hd = headerToJSON hd

headerToJSON :: Header -> Value
headerToJSON (headerName, header) = toJSON (decodeUtf8With lenientDecode . original $ headerName, decodeUtf8With lenientDecode header)

portToJSON :: PortNumber -> Value
portToJSON = toJSON . toInteger

httpVersionToJSON :: HttpVersion -> Value
httpVersionToJSON (HttpVersion major minor) = String $ T.pack (show major) <> "." <> T.pack (show minor)

requestBodyLengthToJSON :: RequestBodyLength -> Value
requestBodyLengthToJSON ChunkedBody = String "Unknown"
requestBodyLengthToJSON (KnownLength l) = toJSON l
