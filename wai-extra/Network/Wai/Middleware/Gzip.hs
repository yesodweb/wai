---------------------------------------------------------

---------------------------------------------------------

-- |
-- Module        : Network.Wai.Middleware.Gzip
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Automatic gzip compression of responses.
module Network.Wai.Middleware.Gzip (
    -- * How to use this module
    -- $howto

    -- ** The Middleware
    -- $gzip
    gzip,

    -- ** The Settings
    -- $settings
    GzipSettings,
    defaultGzipSettings,
    gzipFiles,
    gzipCheckMime,
    gzipSizeThreshold,

    -- ** How to handle file responses
    GzipFiles (..),

    -- ** Miscellaneous
    -- $miscellaneous
    defaultCheckMime,
    def,
) where

import Control.Exception (
    IOException,
    SomeException,
    fromException,
    throwIO,
    try,
 )
import Control.Monad (unless)
import qualified Data.ByteString as S
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Builder.Extra as Blaze (flush)
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.Default.Class (Default (..))
import Data.Function (fix)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Streaming.ByteString.Builder as B
import qualified Data.Streaming.Zlib as Z
import Data.Word8 as W8 (toLower, _semicolon)
import Network.HTTP.Types (
    Header,
    Status (statusCode),
    hContentEncoding,
    hContentLength,
    hContentType,
    hUserAgent,
 )
import Network.HTTP.Types.Header (hAcceptEncoding, hETag, hVary)
import Network.Wai
import Network.Wai.Internal (Response (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified System.IO as IO

import Network.Wai.Header (contentLength, parseQValueList, replaceHeader)
import Network.Wai.Util (splitCommas, trimWS)

-- $howto
--
-- This 'Middleware' adds @gzip encoding@ to an application.
-- Its use is pretty straightforward, but it's good to know
-- how and when it decides to encode the response body.
--
-- A few things to keep in mind when using this middleware:
--
-- * It is advised to put any 'Middleware's that change the
--   response behind this one, because it bases a lot of its
--   decisions on the returned response.
-- * Enabling compression may counteract zero-copy response
--   optimizations on some platforms.
-- * This middleware is applied to every response by default.
--   If it should only encode certain paths,
--   "Network.Wai.Middleware.Routed" might be helpful.

-- $gzip
--
-- There are a good amount of requirements that should be
-- fulfilled before a response will actually be @gzip encoded@
-- by this 'Middleware', so here's a short summary.
--
-- Request requirements:
--
-- * The request needs to accept \"gzip\" in the \"Accept-Encoding\" header.
-- * Requests from Internet Explorer 6 will not be encoded.
--   (i.e. if the request's \"User-Agent\" header contains \"MSIE 6\")
--
-- Response requirements:
--
-- * The response isn't already encoded. (i.e. shouldn't already
--   have a \"Content-Encoding\" header)
-- * The response isn't a @206 Partial Content@ (partial content
--   should never be compressed)
-- * If the response contains a \"Content-Length\" header, it
--   should be larger than the 'gzipSizeThreshold'.
-- * The \"Content-Type\" response header's value should
--   evaluate to 'True' when applied to 'gzipCheckMime'
--   (though 'GzipPreCompressed' will use the \".gz\" file regardless
--   of MIME type on any 'ResponseFile' response)

-- $settings
--
-- If you would like to use the default settings, using just 'def' is enough.
-- The default settings don't compress file responses, only builder and stream
-- responses, and only if the response passes the MIME and length checks. (cf.
-- 'defaultCheckMime' and 'gzipSizeThreshold')
--
-- To customize your own settings, use the 'def' method and set the
-- fields you would like to change as follows:
--
-- @
-- myGzipSettings :: 'GzipSettings'
-- myGzipSettings =
--   'defaultGzipSettings'
--     { 'gzipFiles' = 'GzipCompress'
--     , 'gzipCheckMime' = myMimeCheckFunction
--     , 'gzipSizeThreshold' = 860
--     }
-- @

data GzipSettings = GzipSettings
    { gzipFiles :: GzipFiles
    -- ^ Gzip behavior for files
    --
    -- Only applies to 'ResponseFile' ('responseFile') responses.
    -- So any streamed data will be compressed based solely on the
    -- response headers having the right \"Content-Type\" and
    -- \"Content-Length\". (which are checked with 'gzipCheckMime'
    -- and 'gzipSizeThreshold', respectively)
    , gzipCheckMime :: S.ByteString -> Bool
    -- ^ Decide which files to compress based on MIME type
    --
    -- The 'S.ByteString' is the value of the \"Content-Type\" response
    -- header and will default to 'False' if the header is missing.
    --
    -- E.g. if you'd only want to compress @json@ data, you might
    -- define your own function as follows:
    --
    -- > myCheckMime mime = mime == "application/json"
    , gzipSizeThreshold :: Integer
    -- ^ Skip compression when the size of the response body is
    -- below this amount of bytes (default: 860.)
    --
    -- /Setting this option to less than 150 will actually increase/
    -- /the size of outgoing data if its original size is less than 150 bytes/.
    --
    -- This will only skip compression if the response includes a
    -- \"Content-Length\" header /AND/ the length is less than this
    -- threshold.
    }

-- | Gzip behavior for files.
data GzipFiles
    = -- | Do not compress file ('ResponseFile') responses.
      -- Any 'ResponseBuilder' or 'ResponseStream' might still be compressed.
      GzipIgnore
    | -- | Compress files. Note that this may counteract
      -- zero-copy response optimizations on some platforms.
      GzipCompress
    | -- | Compress files, caching the compressed version in the given directory.
      GzipCacheFolder FilePath
    | -- | Takes the ETag response header into consideration when caching
      -- files in the given folder. If there's no ETag header,
      -- this setting is equivalent to 'GzipCacheFolder'.
      --
      -- N.B. Make sure the 'gzip' middleware is applied before
      -- any 'Middleware' that will set the ETag header.
      --
      -- @since 3.1.12
      GzipCacheETag FilePath
    | -- | If we use compression then try to use the filename with \".gz\"
      -- appended to it. If the file is missing then try next action.
      --
      -- @since 3.0.17
      GzipPreCompressed GzipFiles
    deriving (Show, Eq, Read)

-- $miscellaneous
--
-- 'def' is re-exported for convenience sake, and 'defaultCheckMime'
-- is exported in case anyone wants to use it in defining their own
-- 'gzipCheckMime' function.

-- | Use default MIME settings; /do not/ compress files; skip
-- compression on data smaller than 860 bytes.
instance Default GzipSettings where
    def = defaultGzipSettings

-- | Default settings for the 'gzip' middleware.
--
-- * Does not compress files.
-- * Uses 'defaultCheckMime'.
-- * Compession threshold set to 860 bytes.
--
-- @since 3.1.14.0
defaultGzipSettings :: GzipSettings
defaultGzipSettings = GzipSettings GzipIgnore defaultCheckMime minimumLength

-- | MIME types that will be compressed by default:
-- @text/@ @*@, @application/json@, @application/javascript@,
-- @application/ecmascript@, @image/x-icon@.
defaultCheckMime :: S.ByteString -> Bool
defaultCheckMime bs =
    S8.isPrefixOf "text/" bs || bs' `Set.member` toCompress
  where
    bs' = fst $ S.break (== _semicolon) bs
    toCompress =
        Set.fromList
            [ "application/json"
            , "application/javascript"
            , "application/ecmascript"
            , "image/x-icon"
            ]

-- | Use gzip to compress the body of the response.
gzip :: GzipSettings -> Middleware
gzip set app req sendResponse'
    | skipCompress = app req sendResponse
    | otherwise = app req . checkCompress $ \res ->
        let runAction x = case x of
                (ResponseRaw{}, _) -> sendResponse res
                -- Always skip if 'GzipIgnore'
                (ResponseFile{}, GzipIgnore) -> sendResponse res
                -- If there's a compressed version of the file, we send that.
                (ResponseFile s hs file Nothing, GzipPreCompressed nextAction) ->
                    let compressedVersion = file ++ ".gz"
                     in doesFileExist compressedVersion >>= \y ->
                            if y
                                then sendResponse $ ResponseFile s (fixHeaders hs) compressedVersion Nothing
                                else runAction (ResponseFile s hs file Nothing, nextAction)
                -- Skip if it's not a MIME type we want to compress
                _ | not $ isCorrectMime (responseHeaders res) -> sendResponse res
                -- Use static caching logic
                (ResponseFile s hs file Nothing, GzipCacheFolder cache) ->
                    compressFile s hs file Nothing cache sendResponse
                -- Use static caching logic with "ETag" signatures
                (ResponseFile s hs file Nothing, GzipCacheETag cache) ->
                    let mETag = lookup hETag hs
                     in compressFile s hs file mETag cache sendResponse
                -- Use streaming logic
                _ -> compressE res sendResponse
         in runAction (res, gzipFiles set)
  where
    isCorrectMime =
        maybe False (gzipCheckMime set) . lookup hContentType
    sendResponse = sendResponse' . mapResponseHeaders mAddVary
    acceptEncoding = "Accept-Encoding"
    acceptEncodingLC = "accept-encoding"
    -- Instead of just adding a header willy-nilly, we check if
    -- "Vary" is already present, and add to it if not already included.
    mAddVary [] = [(hVary, acceptEncoding)]
    mAddVary (h@(nm, val) : hs)
        | nm == hVary =
            let vals = splitCommas val
                lowercase = S.map W8.toLower
                -- Field names are case-insensitive, so we lowercase to match
                hasAccEnc = acceptEncodingLC `elem` fmap lowercase vals
                newH
                    | hasAccEnc = h
                    | otherwise = (hVary, acceptEncoding <> ", " <> val)
             in newH : hs
        | otherwise = h : mAddVary hs

    -- Can we skip from just looking at the 'Request'?
    skipCompress =
        not acceptsGZipEncoding || isMSIE6
      where
        reqHdrs = requestHeaders req
        acceptsGZipEncoding =
            maybe False (any isGzip . parseQValueList) $ hAcceptEncoding `lookup` reqHdrs
        isGzip (bs, q) =
            -- We skip if 'q' = Nothing, because it is malformed,
            -- or if it is 0, because that is an explicit "DO NOT USE GZIP"
            bs == "gzip" && maybe False (/= 0) q
        isMSIE6 =
            maybe False ("MSIE 6" `S.isInfixOf`) $ hUserAgent `lookup` reqHdrs

    -- Can we skip just by looking at the current 'Response'?
    checkCompress
        :: (Response -> IO ResponseReceived) -> Response -> IO ResponseReceived
    checkCompress continue res =
        if isEncodedAlready || isPartial || tooSmall
            then sendResponse res
            else continue res
      where
        resHdrs = responseHeaders res
        -- Partial content should NEVER be compressed.
        isPartial = statusCode (responseStatus res) == 206
        isEncodedAlready = isJust $ hContentEncoding `lookup` resHdrs
        tooSmall =
            maybe
                False -- This could be a streaming case
                (< gzipSizeThreshold set)
                $ contentLength resHdrs

-- For a small enough response, gzipping will actually increase the size
-- Potentially for anything less than 860 bytes gzipping could be a net loss
-- The actual number is application specific though and may need to be adjusted
-- http://webmasters.stackexchange.com/questions/31750/what-is-recommended-minimum-object-size-for-gzip-performance-benefits
minimumLength :: Integer
minimumLength = 860

compressFile
    :: Status
    -> [Header]
    -> FilePath
    -> Maybe S.ByteString
    -> FilePath
    -> (Response -> IO a)
    -> IO a
compressFile s hs file mETag cache sendResponse = do
    e <- doesFileExist tmpfile
    if e
        then onSucc
        else do
            createDirectoryIfMissing True cache
            x <- try $
                IO.withBinaryFile file IO.ReadMode $ \inH ->
                    IO.withBinaryFile tmpfile IO.WriteMode $ \outH -> do
                        deflate <- Z.initDeflate 7 $ Z.WindowBits 31
                        -- FIXME this code should write to a temporary file, then
                        -- rename to the final file
                        let goPopper popper = fix $ \loop -> do
                                res <- popper
                                case res of
                                    Z.PRDone -> return ()
                                    Z.PRNext bs -> do
                                        S.hPut outH bs
                                        loop
                                    Z.PRError ex -> throwIO ex
                        fix $ \loop -> do
                            bs <- S.hGetSome inH defaultChunkSize
                            unless (S.null bs) $ do
                                Z.feedDeflate deflate bs >>= goPopper
                                loop
                        goPopper $ Z.finishDeflate deflate
            either onErr (const onSucc) (x :: Either SomeException ())
  where
    onSucc = sendResponse $ responseFile s (fixHeaders hs) tmpfile Nothing
    reportError err =
        IO.hPutStrLn IO.stderr $
            "Network.Wai.Middleware.Gzip: compression failed: " <> err
    onErr e
        -- Catching IOExceptions for file system / hardware oopsies
        | Just ioe <- fromException e = do
            reportError $ show (ioe :: IOException)
            sendResponse $ responseFile s hs file Nothing
        -- Catching ZlibExceptions for compression oopsies
        | Just zlibe <- fromException e = do
            reportError $ show (zlibe :: Z.ZlibException)
            sendResponse $ responseFile s hs file Nothing
        | otherwise = throwIO e

    -- If there's an ETag, use it as the suffix of the cached file.
    eTag = maybe "" (map safe . S8.unpack . trimWS) mETag
    tmpfile = cache ++ '/' : map safe file ++ eTag

    safe c
        | isAsciiUpper c || isAsciiLower c || isDigit c = c
    safe '-' = '-'
    safe '_' = '_'
    safe _ = '_'

compressE
    :: Response
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
compressE res sendResponse =
    wb $ \body -> sendResponse $
        responseStream s (fixHeaders hs) $ \sendChunk flush -> do
            (blazeRecv, _) <- B.newBuilderRecv B.defaultStrategy
            deflate <- Z.initDeflate 1 (Z.WindowBits 31)
            let sendBuilder builder = do
                    popper <- blazeRecv builder
                    fix $ \loop -> do
                        bs <- popper
                        unless (S.null bs) $ do
                            sendBS bs
                            loop
                sendBS bs = Z.feedDeflate deflate bs >>= deflatePopper
                flushBuilder = do
                    sendBuilder Blaze.flush
                    deflatePopper $ Z.flushDeflate deflate
                    flush
                deflatePopper popper = fix $ \loop -> do
                    result <- popper
                    case result of
                        Z.PRDone -> return ()
                        Z.PRNext bs' -> do
                            sendChunk $ byteString bs'
                            loop
                        Z.PRError e -> throwIO e

            body sendBuilder flushBuilder
            sendBuilder Blaze.flush
            deflatePopper $ Z.finishDeflate deflate
  where
    (s, hs, wb) = responseToStream res

-- Remove Content-Length header, since we will certainly have a
-- different length after gzip compression.
fixHeaders :: [Header] -> [Header]
fixHeaders =
    replaceHeader hContentEncoding "gzip" . filter notLength
  where
    notLength (x, _) = x /= hContentLength
