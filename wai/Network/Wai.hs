-- Ignore deprecations, because this module needs to use the deprecated requestBody to construct a response.
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- |
--
-- This module defines a generic web application interface. It is a common
-- protocol between web servers and web applications.
--
-- The overriding design principles here are performance and generality. To
-- address performance, this library uses a streaming interface for request and
-- response bodies, paired with bytestring's 'Builder' type.  The advantages of a
-- streaming API over lazy IO have been debated elsewhere and so will not be
-- addressed here.  However, helper functions like 'responseLBS' allow you to
-- continue using lazy IO if you so desire.
--
-- Generality is achieved by removing many variables commonly found in similar
-- projects that are not universal to all servers. The goal is that the 'Request'
-- object contains only data which is meaningful in all circumstances.
--
-- Please remember when using this package that, while your application may
-- compile without a hitch against many different servers, there are other
-- considerations to be taken when moving to a new backend. For example, if you
-- transfer from a CGI application to a FastCGI one, you might suddenly find you
-- have a memory leak. Conversely, a FastCGI application would be well served to
-- preload all templates from disk when first starting; this would kill the
-- performance of a CGI application.
--
-- This package purposely provides very little functionality. You can find various
-- middlewares, backends and utilities on Hackage. Some of the most commonly used
-- include:
--
-- [warp] <http://hackage.haskell.org/package/warp>
--
-- [wai-extra] <http://hackage.haskell.org/package/wai-extra>
module Network.Wai (
    -- * Types
    Application,
    Middleware,
    ResponseReceived,

    -- * Request
    Request,
    defaultRequest,
    RequestBodyLength (..),

    -- ** Request accessors
    requestMethod,
    httpVersion,
    rawPathInfo,
    rawQueryString,
    requestHeaders,
    isSecure,
    remoteHost,
    pathInfo,
    queryString,
    getRequestBodyChunk,
    requestBody,
    vault,
    requestBodyLength,
    requestHeaderHost,
    requestHeaderRange,
    requestHeaderReferer,
    requestHeaderUserAgent,
    -- $streamingRequestBodies
    strictRequestBody,
    consumeRequestBodyStrict,
    lazyRequestBody,
    consumeRequestBodyLazy,

    -- ** Request modifiers
    setRequestBodyChunks,
    mapRequestHeaders,

    -- * Response
    Response,
    StreamingBody,
    FilePart (..),

    -- ** Response composers
    responseFile,
    responseBuilder,
    responseLBS,
    responseStream,
    responseRaw,

    -- ** Response accessors
    responseStatus,
    responseHeaders,

    -- ** Response modifiers
    responseToStream,
    mapResponseHeaders,
    mapResponseStatus,

    -- * Middleware composition
    ifRequest,
    modifyRequest,
    modifyResponse,
) where

import Control.Monad (unless)
import qualified Data.ByteString as B
import Data.ByteString.Builder (
    Builder,
    byteString,
    lazyByteString,
 )
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified Data.ByteString.Lazy.Internal as LI
import Data.Function (fix)
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr (SockAddrInet))
import Network.Wai.Internal
import qualified System.IO as IO
import System.IO.Unsafe (unsafeInterleaveIO)

----------------------------------------------------------------

-- | Creating 'Response' from a file.
--
-- @since 2.0.0
responseFile
    :: H.Status -> H.ResponseHeaders -> FilePath -> Maybe FilePart -> Response
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
--
-- @since 2.0.0
responseBuilder :: H.Status -> H.ResponseHeaders -> Builder -> Response
responseBuilder = ResponseBuilder

-- | Creating 'Response' from 'L.ByteString'. This is a wrapper for
--   'responseBuilder'.
--
-- @since 0.3.0
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
-- @since 3.0.0
responseStream
    :: H.Status
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
-- @since 2.1.0
responseRaw
    :: (IO B.ByteString -> (B.ByteString -> IO ()) -> IO ())
    -> Response
    -> Response
responseRaw = ResponseRaw

----------------------------------------------------------------

-- | Accessing 'H.Status' in 'Response'.
--
-- @since 1.2.0
responseStatus :: Response -> H.Status
responseStatus (ResponseFile s _ _ _) = s
responseStatus (ResponseBuilder s _ _) = s
responseStatus (ResponseStream s _ _) = s
responseStatus (ResponseRaw _ res) = responseStatus res

-- | Accessing 'H.ResponseHeaders' in 'Response'.
--
-- @since 2.0.0
responseHeaders :: Response -> H.ResponseHeaders
responseHeaders (ResponseFile _ hs _ _) = hs
responseHeaders (ResponseBuilder _ hs _) = hs
responseHeaders (ResponseStream _ hs _) = hs
responseHeaders (ResponseRaw _ res) = responseHeaders res

-- | Converting the body information in 'Response' to a 'StreamingBody'.
--
-- @since 3.0.0
responseToStream
    :: Response
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
--
-- @since 3.0.3.0
mapResponseHeaders
    :: (H.ResponseHeaders -> H.ResponseHeaders) -> Response -> Response
mapResponseHeaders f (ResponseFile s h b1 b2) = ResponseFile s (f h) b1 b2
mapResponseHeaders f (ResponseBuilder s h b) = ResponseBuilder s (f h) b
mapResponseHeaders f (ResponseStream s h b) = ResponseStream s (f h) b
mapResponseHeaders _ r@(ResponseRaw _ _) = r

-- | Apply the provided function to the response status of the Response.
--
-- @since 3.2.1
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
type Application =
    Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived

-- | A default, blank request.
--
-- @since 2.0.0
defaultRequest :: Request
defaultRequest =
    Request
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

-- | A @Middleware@ is a component that sits between the server and application.
--
-- It can modify both the 'Request' and 'Response',
-- to provide simple transformations that are required for all (or most of)
-- your web server’s routes.
--
-- = Users of middleware
--
-- If you are trying to apply one or more 'Middleware's to your 'Application',
-- just call them as functions.
--
-- For example, if you have @corsMiddleware@ and @authorizationMiddleware@,
-- and you want to authorize first, you can do:
--
-- @
-- let allMiddleware app = authorizationMiddleware (corsMiddleware app)
-- @
--
-- to get a new 'Middleware', which first authorizes, then sets, CORS headers.
-- The “outer” middleware is called first.
--
-- You can also chain them via '(.)':
--
-- @
-- let allMiddleware =
--         authorizationMiddleware
--       . corsMiddleware
--       . … more middleware here …
-- @
--
-- Then, once you have an @app :: Application@, you can wrap it
-- in your middleware:
--
-- @
-- let myApp = allMiddleware app :: Application
-- @
--
-- and run it as usual:
--
-- @
-- Warp.run port myApp
-- @
--
-- = Authors of middleware
--
-- When fully expanded, 'Middleware' has the type signature:
--
-- > (Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived) -> Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
--
-- or if we shorten to @type Respond = Response -> IO ResponseReceived@:
--
-- > (Request -> Respond -> IO ResponseReceived) -> Request -> Respond -> IO ResponseReceived
--
-- so a middleware definition takes 3 arguments, an inner application, a request and a response callback.
--
-- Compare with the type of a simple `Application`:
--
-- > Request -> Respond -> IO ResponseReceived
--
-- It takes the 'Request' and @Respond@, but not the extra application.
--
-- Said differently, a middleware has the power of a normal 'Application'
-- — it can inspect the 'Request' and return a 'Response' —
-- but it can (and in many cases it /should/) also call the 'Application' which was passed to it.
--
-- == Modifying the 'Request'
--
-- A lot of middleware just looks at the request and does something based on its values.
--
-- For example, the @authorizationMiddleware@ from above could look at the @Authorization@
-- HTTP header and run <https://jwt.io/ JWT> verification logic against the database.
--
-- @
-- authorizationMiddleware app req respond = do
--   case verifyJWT ('requestHeaders' req) of
--     InvalidJWT err -> respond (invalidJWTResponse err)
--     ValidJWT -> app req respond
-- @
--
-- Notice how the inner app is called when the validation was successful.
-- If it was not, we can respond
-- e.g. with <https://developer.mozilla.org/en-US/docs/Web/HTTP/Status/401 HTTP 401 Unauthorized>,
-- by constructing a 'Response' with 'responseLBS' and passing it to @respond@.
--
-- == Passing arguments to and from your 'Middleware'
--
-- Middleware must often be configurable.
-- Let’s say you have a type @JWTSettings@ that you want to be passed to the middleware.
-- Simply pass an extra argument to your middleware. Then your middleware type turns into:
--
-- @
-- authorizationMiddleware :: JWTSettings -> Application -> Request -> Respond -> IO ResponseReceived
-- authorizationMiddleware jwtSettings req respond =
--   case verifyJWT jwtSettings ('requestHeaders' req) of
--     InvalidJWT err -> respond (invalidJWTResponse err)
--     ValidJWT -> app req respond
-- @
--
-- or alternatively:
--
-- @
-- authorizationMiddleware :: JWTSettings -> Middleware
-- @
--
-- Perhaps less intuitively, you can also /pass on/ data from middleware to the wrapped 'Application':
--
-- @
-- authorizationMiddleware :: JWTSettings -> (JWT -> Application) -> Request -> Respond -> IO ResponseReceived
-- authorizationMiddleware jwtSettings req respond =
--   case verifyJWT jwtSettings ('requestHeaders' req) of
--     InvalidJWT err -> respond (invalidJWTResponse err)
--     ValidJWT jwt -> app jwt req respond
-- @
--
-- although then, chaining different middleware has to take this extra argument into account:
--
-- @
-- let finalApp =
--       authorizationMiddleware
--         (\\jwt -> corsMiddleware
--            (… more middleware here …
--              (app jwt)))
-- @
--
-- == Modifying the 'Response'
--
-- 'Middleware' can also modify the 'Response' that is returned by the inner application.
--
-- This is done by taking the @respond@ callback, using it to define a new @respond'@,
-- and passing this new @respond'@ to the @app@:
--
-- @
-- gzipMiddleware app req respond = do
--   let respond' resp = do
--         resp' <- gzipResponseBody resp
--         respond resp'
--   app req respond'
-- @
--
-- However, modifying the response (especially the response body) is not trivial,
-- so in order to get a sense of how to do it (dealing with the type of 'responseToStream'),
-- it’s best to look at an example, for example <https://hackage.haskell.org/package/wai-extra/docs/src/Network.Wai.Middleware.Gzip.html#gzip the GZIP middleware of wai-extra>.
type Middleware = Application -> Application

-- | Apply a function that modifies a request as a 'Middleware'
--
-- @since 3.2.4
modifyRequest :: (Request -> Request) -> Middleware
modifyRequest f app = app . f

-- | Apply a function that modifies a response as a 'Middleware'
--
-- @since 3.0.3.0
modifyResponse :: (Response -> Response) -> Middleware
modifyResponse f app req respond = app req $ respond . f

-- | Conditionally apply a 'Middleware'
--
-- @since 3.0.3.0
ifRequest :: (Request -> Bool) -> Middleware -> Middleware
ifRequest rpred middle app req
    | rpred req = middle app req
    | otherwise = app req

-- $streamingRequestBodies
--
-- == Streaming Request Bodies
--
-- WAI is designed for streaming in request bodies, which allows you to process them incrementally.
-- You can stream in the request body using functions like 'getRequestBodyChunk',
-- the @wai-conduit@ package, or Yesod's @rawRequestBody@.
--
-- In the normal case, incremental processing is more efficient, since it
-- reduces maximum total memory usage.
-- In the worst case, it helps protect your server against denial-of-service (DOS) attacks, in which
-- an attacker sends huge request bodies to your server.
--
-- Consider these tips to avoid reading the entire request body into memory:
--
-- * Look for library functions that support incremental processing. Sometimes these will use streaming
-- libraries like @conduit@, @pipes@, or @streaming@.
-- * Any attoparsec parser supports streaming input. For an example of this, see the
-- "Data.Conduit.Attoparsec" module in @conduit-extra@.
-- * Consider streaming directly to a file on disk. For an example of this, see the
-- "Data.Conduit.Binary" module in @conduit-extra@.
-- * If you need to direct the request body to multiple destinations, you can stream to both those
-- destinations at the same time.
-- For example, if you wanted to run an HMAC on the request body as well as parse it into JSON,
-- you could use Conduit's @zipSinks@ to send the data to @cryptonite-conduit@'s 'sinkHMAC' and
-- @aeson@'s Attoparsec parser.
-- * If possible, avoid processing large data on your server at all.
-- For example, instead of uploading a file to your server and then to AWS S3,
-- you can have the browser upload directly to S3.
--
-- That said, sometimes it is convenient, or even necessary to read the whole request body into memory.
-- For these purposes, functions like 'strictRequestBody' or 'lazyRequestBody' can be used.
-- When this is the case, consider these strategies to mitigating potential DOS attacks:
--
-- * Set a limit on the request body size you allow.
-- If certain endpoints need larger bodies, whitelist just those endpoints for the large size.
-- Be especially cautious about endpoints that don't require authentication, since these are easier to DOS.
-- You can accomplish this with @wai-extra@'s @requestSizeLimitMiddleware@ or Yesod's @maximumContentLength@.
-- * Consider rate limiting not just on total requests, but also on total bytes sent in.
-- * Consider using services that allow you to identify and blacklist attackers.
-- * Minimize the amount of time the request body stays in memory.
-- * If you need to share request bodies across middleware and your application, you can do so using Wai's 'vault'.
-- If you do this, remove the request body from the vault as soon as possible.
--
-- Warning: Incremental processing will not always be sufficient to prevent a DOS attack.
-- For example, if an attacker sends you a JSON body with a 2MB long string inside,
-- even if you process the body incrementally, you'll still end up with a 2MB-sized 'Text'.
--
-- To mitigate this, employ some of the countermeasures listed above,
-- and try to reject such payloads as early as possible in your codebase.

-- | Get the request body as a lazy ByteString. However, do /not/ use any lazy
-- I\/O, instead reading the entire body into memory strictly.
--
-- Note: Since this function consumes the request body, future calls to it will return the empty string.
--
-- @since 3.0.1
strictRequestBody :: Request -> IO L.ByteString
strictRequestBody req =
    loop id
  where
    loop front = do
        bs <- getRequestBodyChunk req
        if B.null bs
            then return $ front LI.Empty
            else loop (front . LI.Chunk bs)

-- | Synonym for 'strictRequestBody'.
-- This function name is meant to signal the non-idempotent nature of 'strictRequestBody'.
--
-- @since 3.2.3
consumeRequestBodyStrict :: Request -> IO L.ByteString
consumeRequestBodyStrict = strictRequestBody

-- | Get the request body as a lazy ByteString. This uses lazy I\/O under the
-- surface, and therefore all typical warnings regarding lazy I/O apply.
--
-- Note: Since this function consumes the request body, future calls to it will return the empty string.
--
-- @since 1.4.1
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

-- | Synonym for 'lazyRequestBody'.
-- This function name is meant to signal the non-idempotent nature of 'lazyRequestBody'.
--
-- @since 3.2.3
consumeRequestBodyLazy :: Request -> IO L.ByteString
consumeRequestBodyLazy = lazyRequestBody

-- | Apply the provided function to the request header list of the 'Request'.
--
-- @since 3.2.4
mapRequestHeaders
    :: (H.RequestHeaders -> H.RequestHeaders) -> Request -> Request
mapRequestHeaders f request = request{requestHeaders = f (requestHeaders request)}
