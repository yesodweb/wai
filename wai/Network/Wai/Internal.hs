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
data RequestM m = Request {
  -- | Request method such as GET.
     requestMethod        :: H.Method
  -- | HTTP version such as 1.1.
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
  -- | A list of header (a pair of key and value) in an HTTP request.
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
  -- | A request body provided as 'Source'.
  ,  requestBody          :: C.Source m B.ByteString
  -- | A location for arbitrary data to be shared by applications and middleware.
  , vault                 :: Vault
  -- | The size of the request body. In the case of a chunked request body, this may be unknown.
  --
  -- Since 1.4.0
  , requestBodyLength     :: RequestBodyLength
  -- | The value of the Host header in a HTTP request.
  --
  -- Since 2.0.0
  , requestHeaderHost     :: Maybe B.ByteString
  -- | The value of the Range header in a HTTP request.
  --
  -- Since 2.0.0
  , requestHeaderRange   :: Maybe B.ByteString
  }

type Request = RequestM IO

-- | The strange structure of the third field or ResponseSource is to allow for
-- exception-safe resource allocation. As an example:
--
-- > app :: Application
-- > app _ = return $ ResponseSource status200 [] $ \f -> bracket
-- >     (putStrLn "Allocation" >> return 5)
-- >     (\i -> putStrLn $ "Cleaning up: " ++ show i)
-- >     (\_ -> f $ do
-- >         yield $ Chunk $ fromByteString "Hello "
-- >         yield $ Chunk $ fromByteString "World!")
data ResponseM m
    = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
    | ResponseBuilder H.Status H.ResponseHeaders Builder
    | ResponseSource H.Status H.ResponseHeaders (forall b. WithSource m (C.Flush Builder) b)

type Response = ResponseM IO

-- | Auxiliary type for 'ResponseSource'.
type WithSource m a b = (C.Source m a -> m b) -> m b

-- | The size of the request body. In the case of chunked bodies, the size will
-- not be known.
--
-- Since 1.4.0
data RequestBodyLength = ChunkedBody | KnownLength Word64

-- | Information on which part to be sent.
--   Sophisticated application handles Range (and If-Range) then
--   create 'FilePart'.
data FilePart = FilePart
    { filePartOffset    :: Integer
    , filePartByteCount :: Integer
    , filePartFileSize  :: Integer
    } deriving Show
