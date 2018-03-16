{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
-- | Internal constructors and helper functions. Note that no guarantees are
-- given for stability of these interfaces.
module Network.Wai.Internal where

import           Data.ByteString.Builder      (Builder)
import qualified Data.ByteString              as B hiding (pack)
import           Data.Text                    (Text)
import           Data.Typeable                (Typeable)
import           Data.Vault.Lazy              (Vault)
import           Data.Word                    (Word64)
import qualified Network.HTTP.Types           as H
import           Network.Socket               (SockAddr)
import           Data.List                    (intercalate)

-- | Information on the request sent by the client. This abstracts away the
-- details of the underlying implementation.
data Request = Request {
  -- | Request method such as GET.
     requestMethod        :: H.Method
  -- | HTTP version such as 1.1.
  ,  httpVersion          :: H.HttpVersion
  -- | Extra path information sent by the client. The meaning varies slightly
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
  ,  rawPathInfo          :: B.ByteString
  -- | If no query string was specified, this should be empty. This value
  -- /will/ include the leading question mark.
  -- Do not modify this raw value - modify queryString instead.
  ,  rawQueryString       :: B.ByteString
  -- | A list of headers (a pair of key and value) in an HTTP request.
  ,  requestHeaders       :: H.RequestHeaders
  -- | Was this request made over an SSL connection?
  --
  -- Note that this value will /not/ tell you if the client originally made
  -- this request over SSL, but rather whether the current connection is SSL.
  -- The distinction lies with reverse proxies. In many cases, the client will
  -- connect to a load balancer over SSL, but connect to the WAI handler
  -- without SSL. In such a case, 'isSecure' will be 'False', but from a user
  -- perspective, there is a secure connection.
  ,  isSecure             :: Bool
  -- | The client\'s host information.
  ,  remoteHost           :: SockAddr
  -- | Path info in individual pieces - the URL without a hostname/port and
  -- without a query string, split on forward slashes.
  ,  pathInfo             :: [Text]
  -- | Parsed query string information.
  ,  queryString          :: H.Query
  -- | Get the next chunk of the body. Returns 'B.empty' when the
  -- body is fully consumed.
  ,  requestBody          :: IO B.ByteString
  -- | A location for arbitrary data to be shared by applications and middleware.
  ,  vault                 :: Vault
  -- | The size of the request body. In the case of a chunked request body,
  -- this may be unknown.
  --
  -- Since 1.4.0
  ,  requestBodyLength     :: RequestBodyLength
  -- | The value of the Host header in a HTTP request.
  --
  -- Since 2.0.0
  ,  requestHeaderHost     :: Maybe B.ByteString
  -- | The value of the Range header in a HTTP request.
  --
  -- Since 2.0.0
  ,  requestHeaderRange   :: Maybe B.ByteString
  -- | The value of the Referer header in a HTTP request.
  --
  -- Since 3.2.0
  ,  requestHeaderReferer   :: Maybe B.ByteString
  -- | The value of the User-Agent header in a HTTP request.
  --
  -- Since 3.2.0
  ,  requestHeaderUserAgent :: Maybe B.ByteString
  }
  deriving (Typeable)

instance Show Request where
    show Request{..} = "Request {" ++ intercalate ", " [a ++ " = " ++ b | (a,b) <- fields] ++ "}"
        where
            fields =
                [("requestMethod",show requestMethod)
                ,("httpVersion",show httpVersion)
                ,("rawPathInfo",show rawPathInfo)
                ,("rawQueryString",show rawQueryString)
                ,("requestHeaders",show requestHeaders)
                ,("isSecure",show isSecure)
                ,("remoteHost",show remoteHost)
                ,("pathInfo",show pathInfo)
                ,("queryString",show queryString)
                ,("requestBody","<IO ByteString>")
                ,("vault","<Vault>")
                ,("requestBodyLength",show requestBodyLength)
                ,("requestHeaderHost",show requestHeaderHost)
                ,("requestHeaderRange",show requestHeaderRange)
                ]


data Response
    = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
    | ResponseBuilder H.Status H.ResponseHeaders Builder
    | ResponseStream H.Status H.ResponseHeaders StreamingBody
    | ResponseRaw (IO B.ByteString -> (B.ByteString -> IO ()) -> IO ()) Response
  deriving Typeable

-- | Represents a streaming HTTP response body. It's a function of two
-- parameters; the first parameter provides a means of sending another chunk of
-- data, and the second parameter provides a means of flushing the data to the
-- client.
--
-- Since 3.0.0
type StreamingBody = (Builder -> IO ()) -> IO () -> IO ()

-- | The size of the request body. In the case of chunked bodies, the size will
-- not be known.
--
-- Since 1.4.0
data RequestBodyLength = ChunkedBody | KnownLength Word64 deriving Show

-- | Information on which part to be sent.
--   Sophisticated application handles Range (and If-Range) then
--   create 'FilePart'.
data FilePart = FilePart
    { filePartOffset    :: Integer
    , filePartByteCount :: Integer
    , filePartFileSize  :: Integer
    } deriving Show

-- | A special datatype to indicate that the WAI handler has received the
-- response. This is to avoid the need for Rank2Types in the definition of
-- Application.
--
-- It is /highly/ advised that only WAI handlers import and use the data
-- constructor for this data type.
--
-- Since 3.0.0
data ResponseReceived = ResponseReceived
    deriving Typeable
