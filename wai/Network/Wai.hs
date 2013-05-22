{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-|

This module defines a generic web application interface. It is a common
protocol between web servers and web applications.

The overriding design principles here are performance and generality . To
address performance, this library is built on top of the conduit and
blaze-builder packages.  The advantages of conduits over lazy IO have been
debated elsewhere and so will not be addressed here.  However, helper functions
like 'responseLBS' allow you to continue using lazy IO if you so desire.

Generality is achieved by removing many variables commonly found in similar
projects that are not universal to all servers. The goal is that the 'Request'
object contains only data which is meaningful in all circumstances.

Please remember when using this package that, while your application may
compile without a hitch against many different servers, there are other
considerations to be taken when moving to a new backend. For example, if you
transfer from a CGI application to a FastCGI one, you might suddenly find you
have a memory leak. Conversely, a FastCGI application would be well served to
preload all templates from disk when first starting; this would kill the
performance of a CGI application.

This package purposely provides very little functionality. You can find various
middlewares, backends and utilities on Hackage. Some of the most commonly used
include:

[warp] <http://hackage.haskell.org/package/warp>

[wai-extra] <http://hackage.haskell.org/package/wai-extra>

[wai-test] <http://hackage.haskell.org/package/wai-test>

-}
module Network.Wai
    ( -- * WAI interface
      Request (..)
    , Response (..)
    , responseSource
    , Application
    , Middleware
    , FilePart (..)
    , RequestBodyLength (..)
      -- * Response body smart constructors
    , responseLBS
    , responseStatus
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.Typeable (Typeable)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import Blaze.ByteString.Builder (Builder, fromLazyByteString)
import Network.Socket (SockAddr)
import qualified Network.HTTP.Types as H
import Data.Text (Text)
import Data.ByteString.Lazy.Char8 () -- makes it easier to use responseLBS
import Blaze.ByteString.Builder (fromByteString)
import Data.Vault (Vault)
import Data.Word (Word64)

-- | Information on the request sent by the client. This abstracts away the
-- details of the underlying implementation.
data Request = Request
  {  requestMethod  :: H.Method
  ,  httpVersion    :: H.HttpVersion
  -- | Extra path information sent by the client. The meaning varies slightly
  -- depending on backend; in a standalone server setting, this is most likely
  -- all information after the domain name. In a CGI application, this would be
  -- the information following the path to the CGI executable itself.
  -- Do not modify this raw value- modify pathInfo instead.
  ,  rawPathInfo    :: B.ByteString
  -- | If no query string was specified, this should be empty. This value
  -- /will/ include the leading question mark.
  -- Do not modify this raw value- modify queryString instead.
  ,  rawQueryString :: B.ByteString
  -- | Generally the host requested by the user via the Host request header.
  -- Backends are free to provide alternative values as necessary. This value
  -- should not be used to construct URLs.
  ,  serverName     :: B.ByteString
  -- | The listening port that the server received this request on. It is
  -- possible for a server to listen on a non-numeric port (i.e., Unix named
  -- socket), in which case this value will be arbitrary. Like 'serverName',
  -- this value should not be used in URL construction.
  ,  serverPort     :: Int
  ,  requestHeaders :: H.RequestHeaders
  -- | Was this request made over an SSL connection?
  --
  -- This value should /not/ be used, and will be removed in future revisions
  -- of WAI. There is no meaningful way that a backend can indicate whether the
  -- request is actually over a secure channel, due to issues of reverse
  -- proxying.
  ,  isSecure       :: Bool
  -- | The client\'s host information.
  ,  remoteHost     :: SockAddr
  -- | Path info in individual pieces- the url without a hostname/port and without a query string, split on forward slashes,
  ,  pathInfo       :: [Text]
  -- | Parsed query string information
  ,  queryString    :: H.Query
  ,  requestBody    :: C.Source (C.ResourceT IO) B.ByteString
  -- | A location for arbitrary data to be shared by applications and middleware.
  , vault           :: Vault
  -- | The size of the request body. In the case of a chunked request body, this may be unknown.
  --
  -- Since 1.4.0
  , requestBodyLength :: RequestBodyLength
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
    | ResponseSource H.Status H.ResponseHeaders (C.Source (C.ResourceT IO) (C.Flush Builder))
  deriving Typeable

responseStatus :: Response -> H.Status
responseStatus rsp =
    case rsp of
      ResponseFile    s _ _ _ -> s
      ResponseBuilder s _ _   -> s
      ResponseSource  s _ _   -> s

data FilePart = FilePart
    { filePartOffset :: Integer
    , filePartByteCount :: Integer
    } deriving Show

responseSource :: Response -> (H.Status, H.ResponseHeaders, C.Source (C.ResourceT IO) (C.Flush Builder))
responseSource (ResponseSource s h b) = (s, h, b)
responseSource (ResponseFile s h fp (Just part)) =
    (s, h, sourceFilePart part fp C.$= CL.map (C.Chunk . fromByteString))
responseSource (ResponseFile s h fp Nothing) =
    (s, h, CB.sourceFile fp C.$= CL.map (C.Chunk . fromByteString))
responseSource (ResponseBuilder s h b) =
    (s, h, CL.sourceList [C.Chunk b])

sourceFilePart :: C.MonadResource m => FilePart -> FilePath -> C.Source m B.ByteString
sourceFilePart (FilePart offset count) fp =
    CB.sourceFileRange fp (Just offset) (Just count)

responseLBS :: H.Status -> H.ResponseHeaders -> L.ByteString -> Response
responseLBS s h = ResponseBuilder s h . fromLazyByteString

type Application = Request -> C.ResourceT IO Response

-- | Middleware is a component that sits between the server and application. It
-- can do such tasks as GZIP encoding or response caching. What follows is the
-- general definition of middleware, though a middleware author should feel
-- free to modify this.
--
-- As an example of an alternate type for middleware, suppose you write a
-- function to load up session information. The session information is simply a
-- string map \[(String, String)\]. A logical type signatures for this middleware
-- might be:
--
-- @ loadSession :: ([(String, String)] -> Application) -> Application @
--
-- Here, instead of taking a standard 'Application' as its first argument, the
-- middleware takes a function which consumes the session information as well.
type Middleware = Application -> Application

-- | The size of the request body. In the case of chunked bodies, the size will
-- not be known.
--
-- Since 1.4.0
data RequestBodyLength = ChunkedBody | KnownLength Word64
