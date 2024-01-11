{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Internal constructors and helper functions. Note that no guarantees are
-- given for stability of these interfaces.
module Network.Wai.Internal where

import qualified Data.ByteString as B
import Data.ByteString.Builder (Builder)
import Data.List (intercalate)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Data.Vault.Lazy (Vault)
import Data.Word (Word64)
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)

-- | Information on the request sent by the client. This abstracts away the
-- details of the underlying implementation.
{-# DEPRECATED
    requestBody
    "requestBody's name is misleading because it only gets a partial chunk of the body. Use getRequestBodyChunk instead when getting the field, and setRequestBodyChunks when setting the field."
    #-}

data Request = Request
    { requestMethod :: H.Method
    -- ^ Request method such as GET.
    , httpVersion :: H.HttpVersion
    -- ^ HTTP version such as 1.1.
    , rawPathInfo :: B.ByteString
    -- ^ Extra path information sent by the client. The meaning varies slightly
    -- depending on backend; in a standalone server setting, this is most likely
    -- all information after the domain name. In a CGI application, this would be
    -- the information following the path to the CGI executable itself.
    --
    -- Middlewares and routing tools should not modify this raw value, as it may
    -- be used for such things as creating redirect destinations by applications.
    -- Instead, if you are writing a middleware or routing framework, modify the
    -- @pathInfo@ instead. This is the approach taken by systems like Yesod
    -- subsites.
    --
    -- /Note/: At the time of writing this documentation, there is at least one
    -- system (@Network.Wai.UrlMap@ from @wai-extra@) that does not follow the
    -- above recommendation. Therefore, it is recommended that you test the
    -- behavior of your application when using @rawPathInfo@ and any form of
    -- library that might modify the @Request@.
    , rawQueryString :: B.ByteString
    -- ^ If no query string was specified, this should be empty. This value
    -- /will/ include the leading question mark.
    -- Do not modify this raw value - modify queryString instead.
    , requestHeaders :: H.RequestHeaders
    -- ^ A list of headers (a pair of key and value) in an HTTP request.
    , isSecure :: Bool
    -- ^ Was this request made over an SSL connection?
    --
    -- Note that this value will /not/ tell you if the client originally made
    -- this request over SSL, but rather whether the current connection is SSL.
    -- The distinction lies with reverse proxies. In many cases, the client will
    -- connect to a load balancer over SSL, but connect to the WAI handler
    -- without SSL. In such a case, 'isSecure' will be 'False', but from a user
    -- perspective, there is a secure connection.
    , remoteHost :: SockAddr
    -- ^ The client\'s host information.
    , pathInfo :: [Text]
    -- ^ Path info in individual pieces - the URL without a hostname/port and
    -- without a query string, split on forward slashes.
    , queryString :: H.Query
    -- ^ Parsed query string information.
    , requestBody :: IO B.ByteString
    -- ^ Get the next chunk of the body. Returns 'B.empty' when the
    -- body is fully consumed. Since 3.2.2, this is deprecated in favor of 'getRequestBodyChunk'.
    , vault :: Vault
    -- ^ A location for arbitrary data to be shared by applications and middleware.
    , requestBodyLength :: RequestBodyLength
    -- ^ The size of the request body. In the case of a chunked request body,
    -- this may be unknown.
    --
    -- @since 1.4.0
    , requestHeaderHost :: Maybe B.ByteString
    -- ^ The value of the Host header in a HTTP request.
    --
    -- @since 2.0.0
    , requestHeaderRange :: Maybe B.ByteString
    -- ^ The value of the Range header in a HTTP request.
    --
    -- @since 2.0.0
    , requestHeaderReferer :: Maybe B.ByteString
    -- ^ The value of the Referer header in a HTTP request.
    --
    -- @since 3.2.0
    , requestHeaderUserAgent :: Maybe B.ByteString
    -- ^ The value of the User-Agent header in a HTTP request.
    --
    -- @since 3.2.0
    }
    deriving (Typeable)

-- | Get the next chunk of the body. Returns 'B.empty' when the
-- body is fully consumed.
--
-- @since 3.2.2
getRequestBodyChunk :: Request -> IO B.ByteString
getRequestBodyChunk = requestBody

-- | Set the 'requestBody' attribute on a request without triggering a
-- deprecation warning.
--
-- The supplied IO action should return the next chunk of the body each time it
-- is called and 'B.empty' when it has been fully consumed.
--
-- @since 3.2.4
setRequestBodyChunks :: IO B.ByteString -> Request -> Request
setRequestBodyChunks requestBody r =
    r{requestBody = requestBody}

instance Show Request where
    show Request{..} = "Request {" ++ intercalate ", " [a ++ " = " ++ b | (a, b) <- fields] ++ "}"
      where
        fields =
            [ ("requestMethod", show requestMethod)
            , ("httpVersion", show httpVersion)
            , ("rawPathInfo", show rawPathInfo)
            , ("rawQueryString", show rawQueryString)
            , ("requestHeaders", show requestHeaders)
            , ("isSecure", show isSecure)
            , ("remoteHost", show remoteHost)
            , ("pathInfo", show pathInfo)
            , ("queryString", show queryString)
            , ("requestBody", "<IO ByteString>")
            , ("vault", "<Vault>")
            , ("requestBodyLength", show requestBodyLength)
            , ("requestHeaderHost", show requestHeaderHost)
            , ("requestHeaderRange", show requestHeaderRange)
            ]

data Response
    = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
    | ResponseBuilder H.Status H.ResponseHeaders Builder
    | ResponseStream H.Status H.ResponseHeaders StreamingBody
    | ResponseRaw (IO B.ByteString -> (B.ByteString -> IO ()) -> IO ()) Response
    deriving (Typeable)

-- | Represents a streaming HTTP response body. It's a function of two
-- parameters; the first parameter provides a means of sending another chunk of
-- data, and the second parameter provides a means of flushing the data to the
-- client.
--
-- @since 3.0.0
type StreamingBody = (Builder -> IO ()) -> IO () -> IO ()

-- | The size of the request body. In the case of chunked bodies, the size will
-- not be known.
--
-- @since 1.4.0
data RequestBodyLength = ChunkedBody | KnownLength Word64 deriving (Show)

-- | Information on which part to be sent.
--   Sophisticated application handles Range (and If-Range) then
--   create 'FilePart'.
--
-- @since 0.4.0
data FilePart = FilePart
    { filePartOffset :: Integer
    , filePartByteCount :: Integer
    , filePartFileSize :: Integer
    }
    deriving (Show)

-- | A special datatype to indicate that the WAI handler has received the
-- response. This is to avoid the need for Rank2Types in the definition of
-- Application.
--
-- It is /highly/ advised that only WAI handlers import and use the data
-- constructor for this data type.
--
-- @since 3.0.0
data ResponseReceived = ResponseReceived
    deriving (Typeable)
