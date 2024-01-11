{-# LANGUAGE CPP #-}

module Network.Wai.Middleware.RequestLogger.JSON (
    formatAsJSON,
    formatAsJSONWithHeaders,
    requestToJSON,
) where

import Data.Aeson
import qualified Data.ByteString.Builder as BB (toLazyByteString)
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Lazy (toStrict)
import Data.CaseInsensitive (original)
import Data.IP (fromHostAddress, fromIPv4)
import Data.Maybe (maybeToList)
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid ((<>))
#endif
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Time (NominalDiffTime)
import Data.Word (Word32)
import Network.HTTP.Types as H
import Network.Socket (PortNumber, SockAddr (..))
import Network.Wai
import System.Log.FastLogger (toLogStr)
import Text.Printf (printf)

import Network.Wai.Middleware.RequestLogger

formatAsJSON :: OutputFormatterWithDetails
formatAsJSON date req status responseSize duration reqBody response =
    toLogStr
        ( encode $
            object
                [ "request" .= requestToJSON req reqBody (Just duration)
                , "response"
                    .= object
                        [ "status" .= statusCode status
                        , "size" .= responseSize
                        , "body"
                            .= if statusCode status >= 400
                                then Just . decodeUtf8With lenientDecode . toStrict . BB.toLazyByteString $ response
                                else Nothing
                        ]
                , "time" .= decodeUtf8With lenientDecode date
                ]
        )
        <> "\n"

-- | Same as @formatAsJSON@ but with response headers included
--
-- This is useful for passing arbitrary data from your application out to the
-- WAI layer for it to be logged, but you may need to be careful to
-- subsequently redact any headers which may contain sensitive data.
--
-- @since 3.0.27
formatAsJSONWithHeaders :: OutputFormatterWithDetailsAndHeaders
formatAsJSONWithHeaders date req status resSize duration reqBody res resHeaders =
    toLogStr
        ( encode $
            object
                [ "request" .= requestToJSON req reqBody (Just duration)
                , "response"
                    .= object
                        [ "status" .= statusCode status
                        , "size" .= resSize
                        , "headers" .= responseHeadersToJSON resHeaders
                        , "body"
                            .= if statusCode status >= 400
                                then Just . decodeUtf8With lenientDecode . toStrict . BB.toLazyByteString $ res
                                else Nothing
                        ]
                , "time" .= decodeUtf8With lenientDecode date
                ]
        )
        <> "\n"

word32ToHostAddress :: Word32 -> Text
word32ToHostAddress = T.intercalate "." . map (T.pack . show) . fromIPv4 . fromHostAddress

readAsDouble :: String -> Double
readAsDouble = read

-- | Get the JSON representation for a request
--
-- This representation is identical to that used in 'formatAsJSON' for the
-- request. It includes:
--
--   [@method@]:
--   [@path@]:
--   [@queryString@]:
--   [@size@]: The size of the body, as defined in the request. This may differ
--   from the size of the data passed in the second argument.
--   [@body@]: The body, concatenated directly from the chunks passed in
--   [@remoteHost@]:
--   [@httpVersion@]:
--   [@headers@]:
--
-- If a @'Just' duration@ is passed in, then additionally the JSON includes:
--
--   [@durationMs@] The duration, formatted in milliseconds, to 2 decimal
--   places
--
-- This representation is not an API, and may change at any time (within reason)
-- without a major version bump.
--
-- @since 3.1.4
requestToJSON
    :: Request
    -- ^ The WAI request
    -> [S8.ByteString]
    -- ^ Chunked request body
    -> Maybe NominalDiffTime
    -- ^ Optional request duration
    -> Value
requestToJSON req reqBody duration =
    object $
        [ "method" .= decodeUtf8With lenientDecode (requestMethod req)
        , "path" .= decodeUtf8With lenientDecode (rawPathInfo req)
        , "queryString" .= map queryItemToJSON (queryString req)
        , "size" .= requestBodyLengthToJSON (requestBodyLength req)
        , "body" .= decodeUtf8With lenientDecode (S8.concat reqBody)
        , "remoteHost" .= sockToJSON (remoteHost req)
        , "httpVersion" .= httpVersionToJSON (httpVersion req)
        , "headers" .= requestHeadersToJSON (requestHeaders req)
        ]
            <> maybeToList
                ( ("durationMs" .=)
                    . readAsDouble
                    . printf "%.2f"
                    . rationalToDouble
                    . (* 1000)
                    . toRational
                    <$> duration
                )
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
    object ["unix" .= sock]
#if !MIN_VERSION_network(3,0,0)
sockToJSON (SockAddrCan i) =
  object [ "can" .= i ]
#endif

queryItemToJSON :: QueryItem -> Value
queryItemToJSON (name, mValue) =
    toJSON
        (decodeUtf8With lenientDecode name, fmap (decodeUtf8With lenientDecode) mValue)

requestHeadersToJSON :: RequestHeaders -> Value
requestHeadersToJSON = toJSON . map hToJ
  where
    -- Redact cookies
    hToJ ("Cookie", _) = toJSON ("Cookie" :: Text, "-RDCT-" :: Text)
    hToJ hd = headerToJSON hd

responseHeadersToJSON :: [Header] -> Value
responseHeadersToJSON = toJSON . map hToJ
  where
    -- Redact cookies
    hToJ ("Set-Cookie", _) = toJSON ("Set-Cookie" :: Text, "-RDCT-" :: Text)
    hToJ hd = headerToJSON hd

headerToJSON :: Header -> Value
headerToJSON (headerName, header) =
    toJSON
        ( decodeUtf8With lenientDecode . original $ headerName
        , decodeUtf8With lenientDecode header
        )

portToJSON :: PortNumber -> Value
portToJSON = toJSON . toInteger

httpVersionToJSON :: HttpVersion -> Value
httpVersionToJSON (HttpVersion major minor) = String $ T.pack (show major) <> "." <> T.pack (show minor)

requestBodyLengthToJSON :: RequestBodyLength -> Value
requestBodyLengthToJSON ChunkedBody = String "Unknown"
requestBodyLengthToJSON (KnownLength l) = toJSON l
