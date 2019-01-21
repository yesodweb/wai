{-|

This module defines a generic web application interface. It is a common
protocol between web servers and web applications.

The overriding design principles here are performance and generality. To
address performance, this library uses a streaming interface for request and
response bodies, paired with bytestring's 'Builder' type.  The advantages of a
streaming API over lazy IO have been debated elsewhere and so will not be
addressed here.  However, helper functions like 'responseLBS' allow you to
continue using lazy IO if you so desire.

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
-- Ignore deprecations, because this module needs to use the deprecated requestBody to construct a response.
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Network.Wai
    (
      -- * Types
      Application
    , Middleware
    , ResponseReceived
      -- * Request
    , Request
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
    , requestHeaderReferer
    , requestHeaderUserAgent
    , strictRequestBody
    , lazyRequestBody
      -- * Response
    , Response
    , StreamingBody
    , FilePart (..)
      -- ** Response composers
    , responseFile
    , responseBuilder
    , responseLBS
    , responseStream
    , responseRaw
      -- ** Response accessors
    , responseStatus
    , responseHeaders
      -- ** Response modifiers
    , responseToStream
    , mapResponseHeaders
    , mapResponseStatus
      -- * Middleware composition
    , ifRequest
    , modifyResponse
    ) where

import           Data.ByteString.Builder      (Builder, lazyByteString)
import           Data.ByteString.Builder      (byteString)
import           Control.Monad                (unless)
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as L
import qualified Data.ByteString.Lazy.Internal as LI
import           Data.ByteString.Lazy.Internal (defaultChunkSize)
import           Data.ByteString.Lazy.Char8   ()
import           Data.Function                (fix)
import qualified Network.HTTP.Types           as H
import           Network.Socket               (SockAddr (SockAddrInet))
import           Network.Wai.Internal
import qualified System.IO                    as IO
import           System.IO.Unsafe             (unsafeInterleaveIO)

----------------------------------------------------------------

-- | Creating 'Response' from a file.
responseFile :: H.Status -> H.ResponseHeaders -> FilePath -> Maybe FilePart -> Response
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
-- they are already large, then an insert operation is used
-- to avoid copying.
--
-- Q3. Doesn't this prevent us from creating comet-style servers, since data
-- will be cached?
--
-- A3. You can force a Builder to output a ByteString before it is an
-- optimal size by sending a flush command.
responseBuilder :: H.Status -> H.ResponseHeaders -> Builder -> Response
responseBuilder = ResponseBuilder

-- | Creating 'Response' from 'L.ByteString'. This is a wrapper for
--   'responseBuilder'.
responseLBS :: H.Status -> H.ResponseHeaders -> L.ByteString -> Response
responseLBS s h = ResponseBuilder s h . lazyByteString

-- | Creating 'Response' from a stream of values.
--
-- In order to allocate resources in an exception-safe manner, you can use the
-- @bracket@ pattern outside of the call to @responseStream@. As a trivial
-- example:
--
-- @
-- app :: Application
-- app req respond = bracket_
--     (putStrLn \"Allocating scarce resource\")
--     (putStrLn \"Cleaning up\")
--     $ respond $ responseStream status200 [] $ \\write flush -> do
--         write $ byteString \"Hello\\n\"
--         flush
--         write $ byteString \"World\\n\"
-- @
--
-- Note that in some cases you can use @bracket@ from inside @responseStream@
-- as well. However, placing the call on the outside allows your status value
-- and response headers to depend on the scarce resource.
--
-- Since 3.0.0
responseStream :: H.Status
               -> H.ResponseHeaders
               -> StreamingBody
               -> Response
responseStream = ResponseStream

-- | Create a response for a raw application. This is useful for \"upgrade\"
-- situations such as WebSockets, where an application requests for the server
-- to grant it raw network access.
--
-- This function requires a backup response to be provided, for the case where
-- the handler in question does not support such upgrading (e.g., CGI apps).
--
-- In the event that you read from the request body before returning a
-- @responseRaw@, behavior is undefined.
--
-- Since 2.1.0
responseRaw :: (IO B.ByteString -> (B.ByteString -> IO ()) -> IO ())
            -> Response
            -> Response
responseRaw = ResponseRaw

----------------------------------------------------------------

-- | Accessing 'H.Status' in 'Response'.
responseStatus :: Response -> H.Status
responseStatus (ResponseFile    s _ _ _) = s
responseStatus (ResponseBuilder s _ _  ) = s
responseStatus (ResponseStream  s _ _  ) = s
responseStatus (ResponseRaw _ res      ) = responseStatus res

-- | Accessing 'H.ResponseHeaders' in 'Response'.
responseHeaders :: Response -> H.ResponseHeaders
responseHeaders (ResponseFile    _ hs _ _) = hs
responseHeaders (ResponseBuilder _ hs _  ) = hs
responseHeaders (ResponseStream  _ hs _  ) = hs
responseHeaders (ResponseRaw _ res)        = responseHeaders res

-- | Converting the body information in 'Response' to a 'StreamingBody'.
responseToStream :: Response
                 -> ( H.Status
                    , H.ResponseHeaders
                    , (StreamingBody -> IO a) -> IO a
                    )
responseToStream (ResponseStream s h b) = (s, h, ($ b))
responseToStream (ResponseFile s h fp (Just part)) =
    ( s
    , h
    , \withBody -> IO.withBinaryFile fp IO.ReadMode $ \handle -> withBody $ \sendChunk _flush -> do
        IO.hSeek handle IO.AbsoluteSeek $ filePartOffset part
        let loop remaining | remaining <= 0 = return ()
            loop remaining = do
                bs <- B.hGetSome handle defaultChunkSize
                unless (B.null bs) $ do
                    let x = B.take remaining bs
                    sendChunk $ byteString x
                    loop $ remaining - B.length x
        loop $ fromIntegral $ filePartByteCount part
    )
responseToStream (ResponseFile s h fp Nothing) =
    ( s
    , h
    , \withBody -> IO.withBinaryFile fp IO.ReadMode $ \handle ->
       withBody $ \sendChunk _flush -> fix $ \loop -> do
            bs <- B.hGetSome handle defaultChunkSize
            unless (B.null bs) $ do
                sendChunk $ byteString bs
                loop
    )
responseToStream (ResponseBuilder s h b) =
    (s, h, \withBody -> withBody $ \sendChunk _flush -> sendChunk b)
responseToStream (ResponseRaw _ res) = responseToStream res

-- | Apply the provided function to the response header list of the Response.
mapResponseHeaders :: (H.ResponseHeaders -> H.ResponseHeaders) -> Response -> Response
mapResponseHeaders f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapResponseHeaders f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapResponseHeaders f (ResponseStream s h b) = ResponseStream s (f h) b
mapResponseHeaders _ r@(ResponseRaw _ _) = r

-- | Apply the provided function to the response status of the Response.
mapResponseStatus :: (H.Status -> H.Status) -> Response -> Response
mapResponseStatus f (ResponseFile s h b1 b2) = ResponseFile (f s) h b1 b2
mapResponseStatus f (ResponseBuilder s h b) = ResponseBuilder (f s) h b
mapResponseStatus f (ResponseStream s h b) = ResponseStream (f s) h b
mapResponseStatus _ r@(ResponseRaw _ _) = r

----------------------------------------------------------------

-- | The WAI application.
--
-- Note that, since WAI 3.0, this type is structured in continuation passing
-- style to allow for proper safe resource handling. This was handled in the
-- past via other means (e.g., @ResourceT@). As a demonstration:
--
-- @
-- app :: Application
-- app req respond = bracket_
--     (putStrLn \"Allocating scarce resource\")
--     (putStrLn \"Cleaning up\")
--     (respond $ responseLBS status200 [] \"Hello World\")
-- @
type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived


-- | A default, blank request.
--
-- Since 2.0.0
defaultRequest :: Request
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
    , requestBody = return B.empty
    , vault = mempty
    , requestBodyLength = KnownLength 0
    , requestHeaderHost = Nothing
    , requestHeaderRange = Nothing
    , requestHeaderReferer = Nothing
    , requestHeaderUserAgent = Nothing
    }


-- | Middleware is a component that sits between the server and application. It
-- can do such tasks as GZIP encoding or response caching. What follows is the
-- general definition of middleware, though a middleware author should feel
-- free to modify this.
--
-- As an example of an alternate type for middleware, suppose you write a
-- function to load up session information. The session information is simply a
-- string map \[(String, String)\]. A logical type signature for this middleware
-- might be:
--
-- @ loadSession :: ([(String, String)] -> Application) -> Application @
--
-- Here, instead of taking a standard 'Application' as its first argument, the
-- middleware takes a function which consumes the session information as well.
type Middleware = Application -> Application


-- | apply a function that modifies a response as a 'Middleware'
modifyResponse :: (Response -> Response) -> Middleware
modifyResponse f app req respond = app req $ respond . f


-- | conditionally apply a 'Middleware'
ifRequest :: (Request -> Bool) -> Middleware -> Middleware
ifRequest rpred middle app req | rpred req = middle app req
                               | otherwise =        app req



-- | Get the request body as a lazy ByteString. However, do /not/ use any lazy
-- I\/O, instead reading the entire body into memory strictly.
--
-- Since 3.0.1
strictRequestBody :: Request -> IO L.ByteString
strictRequestBody req =
    loop id
  where
    loop front = do
        bs <- getRequestBodyChunk req
        if B.null bs
            then return $ front LI.Empty
            else loop (front . LI.Chunk bs)

-- | Get the request body as a lazy ByteString. This uses lazy I\/O under the
-- surface, and therefore all typical warnings regarding lazy I/O apply.
--
-- Since 1.4.1
lazyRequestBody :: Request -> IO L.ByteString
lazyRequestBody req =
    loop
  where
    loop = unsafeInterleaveIO $ do
        bs <- getRequestBodyChunk req
        if B.null bs
            then return LI.Empty
            else do
                bss <- loop
                return $ LI.Chunk bs bss
