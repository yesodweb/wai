{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
-- | Internal constructors and helper functions. Note that no guarantees are
-- given for stability of these interfaces.
module Network.Wai.Internal where

import           Blaze.ByteString.Builder     (Builder)
import qualified Data.ByteString              as B
import qualified Data.Conduit                 as C
import           Data.Text                    (Text)
import           Data.Typeable                (Typeable)
#if MIN_VERSION_vault(0,3,0)
import Data.Vault.Lazy (Vault)
#else
import Data.Vault (Vault)
#endif
import           Data.Word                    (Word64)
import qualified Network.HTTP.Types           as H
import           Network.Socket               (SockAddr)

-- | Information on the request sent by the client. This abstracts away the
-- details of the underlying implementation.
data Request = Request
  {  requestMethod        :: H.Method
  ,  httpVersion          :: H.HttpVersion
  -- | Extra path information sent by the client. The meaning varies slightly
  -- depending on backend; in a standalone server setting, this is most likely
  -- all information after the domain name. In a CGI application, this would be
  -- the information following the path to the CGI executable itself.
  -- Do not modify this raw value- modify pathInfo instead.
  ,  rawPathInfo          :: B.ByteString
  -- | If no query string was specified, this should be empty. This value
  -- /will/ include the leading question mark.
  -- Do not modify this raw value- modify queryString instead.
  ,  rawQueryString       :: B.ByteString
  ,  requestHeaders       :: H.RequestHeaders
  -- | Was this request made over an SSL connection?
  --
  -- Note that this value will /not/ tell you if the client originally made
  -- this request over SSL, but rather whether the current connection is SSL.
  -- The distinction lies with reverse proxies. In many cases, the client will
  -- connect to a load balancer over SSL, but connect to the WAI handler
  -- without SSL. In such a case, @isSecure@ will be @False@, but from a user
  -- perspective, there is a secure connection.
  ,  isSecure             :: Bool
  -- | The client\'s host information.
  ,  remoteHost           :: SockAddr
  -- | Path info in individual pieces- the url without a hostname/port and without a query string, split on forward slashes,
  ,  pathInfo             :: [Text]
  -- | Parsed query string information
  ,  queryString          :: H.Query
  ,  requestBody          :: C.Source IO B.ByteString
  -- | A location for arbitrary data to be shared by applications and middleware.
  , vault                 :: Vault
  -- | The size of the request body. In the case of a chunked request body, this may be unknown.
  --
  -- Since 1.4.0
  , requestBodyLength     :: RequestBodyLength
  }
  deriving (Typeable)

-- |
--
-- Some questions and answers about the usage of 'Builder' here:
--
-- Q1. Shouldn't it be at the user's discretion to use Builders internally and
-- then create a stream of ByteStrings?
--
-- A1. That would be less efficient, as we wouldn't get cheap concatenation
-- with the response headers.
--
-- Q2. Isn't it really inefficient to convert from ByteString to Builder, and
-- then right back to ByteString?
--
-- A2. No. If the ByteStrings are small, then they will be copied into a larger
-- buffer, which should be a performance gain overall (less system calls). If
-- they are already large, then blaze-builder uses an InsertByteString
-- instruction to avoid copying.
--
-- Q3. Doesn't this prevent us from creating comet-style servers, since data
-- will be cached?
--
-- A3. You can force blaze-builder to output a ByteString before it is an
-- optimal size by sending a flush command.
data Response
    = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
    | ResponseBuilder H.Status H.ResponseHeaders Builder
    | ResponseSource H.Status H.ResponseHeaders (forall b. WithSource IO (C.Flush Builder) b)
  deriving Typeable

type WithSource m a b = (C.Source m a -> m b) -> m b

-- | The size of the request body. In the case of chunked bodies, the size will
-- not be known.
--
-- Since 1.4.0
data RequestBodyLength = ChunkedBody | KnownLength Word64

data FilePart = FilePart
    { filePartOffset    :: Integer
    , filePartByteCount :: Integer
    , filePartFileSize  :: Integer
    } deriving Show
