{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
-- | Internal constructors and helper functions. Note that no guarantees are
-- given for stability of these interfaces.
module Network.Wai.Internal where

import           Blaze.ByteString.Builder     (Builder)
import           Control.Exception            (IOException, try)
import qualified Data.ByteString              as B hiding (pack)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Char8        as B (pack, readInteger)
import qualified Data.ByteString.Lazy as L
import           Data.Maybe                   (listToMaybe)
import           Data.Text                    (Text)
import           Data.Typeable                (Typeable)
import           Data.Vault.Lazy              (Vault)
import           Data.Word                    (Word64)
import qualified Network.HTTP.Types           as H
import qualified Network.HTTP.Types.Header as HH
import           Network.Socket               (SockAddr)
import           Numeric                      (showInt)
import           Data.List                    (intercalate)
import qualified System.PosixCompat.Files     as P

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

-- | Look up the size of a file in 'Right' or the 'IOException' in 'Left'.
tryGetFileSize :: FilePath -> IO (Either IOException Integer)
tryGetFileSize path =
    fmap (fromIntegral . P.fileSize) <$> try (P.getFileStatus path)

-- | \"Content-Range\".
hContentRange :: H.HeaderName
hContentRange = "Content-Range"

-- | @contentRangeHeader beg end total@ constructs a Content-Range 'H.Header'
-- for the range specified.
contentRangeHeader :: Integer -> Integer -> Integer -> H.Header
contentRangeHeader beg end total = (hContentRange, range)
  where
    range = B.pack
      -- building with ShowS
      $ 'b' : 'y': 't' : 'e' : 's' : ' '
      : (if beg > end then ('*':) else
          showInt beg
          . ('-' :)
          . showInt end)
      ( '/'
      : showInt total "")

-- | Given the full size of a file and optionally a Range header value,
-- determine the range to serve by parsing the range header and obeying it, or
-- serving the whole file if it's absent or malformed.
chooseFilePart :: Integer -> Maybe B.ByteString -> FilePart
chooseFilePart size Nothing      = FilePart 0 size size
chooseFilePart size (Just range) = case parseByteRanges range >>= listToMaybe of
    -- Range is broken
    Nothing -> FilePart 0 size size
    Just hrange -> checkRange hrange
  where
    checkRange (H.ByteRangeFrom   beg)     = fromRange beg (size - 1)
    checkRange (H.ByteRangeFromTo beg end) = fromRange beg (min (size - 1) end)
    checkRange (H.ByteRangeSuffix count)   = fromRange (max 0 (size - count)) (size - 1)

    fromRange beg end = FilePart beg (end - beg + 1) size

-- | Adjust the given 'H.Status' and 'H.ResponseHeaders' based on the given
-- 'FilePart'.  This means replacing the status with 206 if the response is
-- partial, and adding the Content-Length (always) and Content-Range (if
-- appropriate) headers.
adjustForFilePart :: H.Status -> H.ResponseHeaders -> FilePart -> (H.Status, H.ResponseHeaders)
adjustForFilePart s h part = (s', h'')
  where
    off = filePartOffset part
    len = filePartByteCount part
    size = filePartFileSize part

    contentRange = contentRangeHeader off (off + len - 1) size
    lengthBS = L.toStrict $ B.toLazyByteString $ B.integerDec len
    s' = if filePartByteCount part /= size then H.partialContent206 else s
    h' = (H.hContentLength, lengthBS):h
    h'' = (if len == size then id else (contentRange:)) h'

-- | Parse the value of a Range header into a 'HH.ByteRanges'.
parseByteRanges :: B.ByteString -> Maybe HH.ByteRanges
parseByteRanges bs1 = do
    bs2 <- stripPrefix "bytes=" bs1
    (r, bs3) <- range bs2
    ranges (r:) bs3
  where
    range bs2 = do
        (i, bs3) <- B.readInteger bs2
        if i < 0 -- has prefix "-" ("-0" is not valid, but here treated as "0-")
            then Just (HH.ByteRangeSuffix (negate i), bs3)
            else do
                bs4 <- stripPrefix "-" bs3
                case B.readInteger bs4 of
                    Just (j, bs5) | j >= i -> Just (HH.ByteRangeFromTo i j, bs5)
                    _ -> Just (HH.ByteRangeFrom i, bs4)
    ranges front bs3
        | B.null bs3 = Just (front [])
        | otherwise = do
            bs4 <- stripPrefix "," bs3
            (r, bs5) <- range bs4
            ranges (front . (r:)) bs5

    stripPrefix x y
        | x `B.isPrefixOf` y = Just (B.drop (B.length x) y)
        | otherwise = Nothing
