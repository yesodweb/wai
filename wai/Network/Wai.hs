{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE CPP #-}
{-|

This module defines a generic web application interface. It is a common
protocol between web servers and web applications.

The overriding design principles here are performance and generality. To
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
    (
      -- * Types
      Application, ApplicationM
    , Middleware
      -- * Request
    , RequestM, Request
    , defaultRequest
    , RequestBodyLength (..)
      -- ** Request accessors
    , requestMethod
    , httpVersion
    , rawPathInfo
    , rawQueryString
    , requestHeaders
    , isSecure
    , remoteHost
    , pathInfo
    , queryString
    , requestBody
    , vault
    , requestBodyLength
    , requestHeaderHost
    , requestHeaderRange
    , lazyRequestBody
      -- * Response
    , Response, ResponseM
    , FilePart (..)
    , WithSource
      -- ** ResponseM composers
    , responseFile
    , responseBuilder
    , responseLBS
    , responseSource
    , responseSourceBracket
      -- * ResponseM accessors
    , responseStatus
    , responseHeaders
    , responseToSource
    ) where

import           Blaze.ByteString.Builder     (Builder, fromLazyByteString)
import           Blaze.ByteString.Builder     (fromByteString)
import           Control.Exception            (bracket, bracketOnError)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as L
import           Data.ByteString.Lazy.Char8   ()
import qualified Data.Conduit                 as C
import qualified Data.Conduit.Binary          as CB
import           Data.Conduit.Lazy            (lazyConsume)
import qualified Data.Conduit.List            as CL
import           Data.Monoid                  (mempty)
import qualified Network.HTTP.Types           as H
import           Network.Socket               (SockAddr (SockAddrInet))
import           Network.Wai.Internal
import qualified System.IO                    as IO

----------------------------------------------------------------

-- | Creating 'Response' from a file.
responseFile :: H.Status -> H.ResponseHeaders -> FilePath -> Maybe FilePart -> ResponseM m
responseFile = ResponseFile

-- | Creating 'Response' from 'Builder'.
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
responseBuilder :: H.Status -> H.ResponseHeaders -> Builder -> ResponseM m
responseBuilder = ResponseBuilder

-- | Creating 'Response' from 'L.ByteString'. This is a wrapper for
--   'responseBuilder'.
responseLBS :: H.Status -> H.ResponseHeaders -> L.ByteString -> ResponseM m
responseLBS s h = ResponseBuilder s h . fromLazyByteString

-- | Creating 'Response' from 'C.Source'.
responseSource :: H.Status -> H.ResponseHeaders -> C.Source m (C.Flush Builder) -> ResponseM m
responseSource st hs src = ResponseSource st hs ($ src)

-- | Creating 'Response' with allocated resource safely released.
--
--   * The first argument is an action to allocate resource.
--
--   * The second argument is a function to release the resource.
--
--   * The third argument is a function to create
--     ('H.Status','H.ResponseHeaders','C.Source' 'IO' ('C.Flush' 'Builder'))
--     from the resource.
responseSourceBracket :: IO a
                      -> (a -> IO ())
                      -> (a -> IO (H.Status
                                  ,H.ResponseHeaders
                                  ,C.Source IO (C.Flush Builder)))
                      -> IO (ResponseM IO)
responseSourceBracket setup teardown action =
    bracketOnError setup teardown $ \resource -> do
        (st,hdr,src) <- action resource
        return $ ResponseSource st hdr $ \f ->
            bracket (return resource) teardown (\_ -> f src)

----------------------------------------------------------------

-- | Accessing 'H.Status' in 'Response'.
responseStatus :: ResponseM m -> H.Status
responseStatus (ResponseFile    s _ _ _) = s
responseStatus (ResponseBuilder s _ _  ) = s
responseStatus (ResponseSource  s _ _  ) = s

-- | Accessing 'H.Status' in 'Response'.
responseHeaders :: ResponseM m -> H.ResponseHeaders
responseHeaders (ResponseFile    _ hs _ _) = hs
responseHeaders (ResponseBuilder _ hs _  ) = hs
responseHeaders (ResponseSource  _ hs _  ) = hs

-- | Converting the body information in 'Response' to 'Source'.
responseToSource :: ResponseM IO
                 -> (H.Status, H.ResponseHeaders, WithSource IO (C.Flush Builder) b)
responseToSource (ResponseSource s h b) = (s, h, b)
responseToSource (ResponseFile s h fp (Just part)) =
    (s, h, \f -> IO.withFile fp IO.ReadMode $ \handle -> f $ sourceFilePart handle part C.$= CL.map (C.Chunk . fromByteString))
responseToSource (ResponseFile s h fp Nothing) =
    (s, h, \f -> IO.withFile fp IO.ReadMode $ \handle -> f $ CB.sourceHandle handle C.$= CL.map (C.Chunk . fromByteString))
responseToSource (ResponseBuilder s h b) =
    (s, h, ($ CL.sourceList [C.Chunk b]))

sourceFilePart :: IO.Handle -> FilePart -> C.Source IO B.ByteString
sourceFilePart handle (FilePart offset count _) =
    CB.sourceHandleRange handle (Just offset) (Just count)

----------------------------------------------------------------

-- | The WAI application.
type ApplicationM m = RequestM m -> m (ResponseM m)

type Application = ApplicationM IO

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
type MiddlewareM m = ApplicationM m -> ApplicationM m

type Middleware = MiddlewareM IO

-- | A default, blank request.
--
-- Since 2.0.0
defaultRequest :: Monad m => RequestM m
defaultRequest = Request
    { requestMethod = H.methodGet
    , httpVersion = H.http10
    , rawPathInfo = B.empty
    , rawQueryString = B.empty
    , requestHeaders = []
    , isSecure = False
    , remoteHost = SockAddrInet 0 0
    , pathInfo = []
    , queryString = []
    , requestBody = return ()
    , vault = mempty
    , requestBodyLength = KnownLength 0
    , requestHeaderHost = Nothing
    , requestHeaderRange = Nothing
    }

-- | Get the request body as a lazy ByteString. This uses lazy I\/O under the
-- surface, and therefore all typical warnings regarding lazy I/O apply.
--
-- Since 1.4.1
lazyRequestBody :: RequestM IO -> IO L.ByteString
lazyRequestBody = fmap L.fromChunks . lazyConsume . requestBody

