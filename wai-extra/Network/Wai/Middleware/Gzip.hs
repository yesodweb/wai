{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
--
---------------------------------------------------------
module Network.Wai.Middleware.Gzip
    ( gzip
    , GzipSettings
    , gzipFiles
    , GzipFiles (..)
    , gzipCheckMime
    , def
    , defaultCheckMime
    ) where

import Control.Exception (SomeException, throwIO, try)
import Control.Monad (unless)
import qualified Data.ByteString as S
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Builder.Extra as Blaze (flush)
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Data.Default.Class (Default (..))
import Data.Function (fix)
import Data.Maybe (isJust)
import qualified Data.Set as Set
import qualified Data.Streaming.ByteString.Builder as B
import qualified Data.Streaming.Zlib as Z
import Data.Word8 (_comma, _semicolon, _space)
import Network.HTTP.Types (
    Header,
    Status,
    hContentEncoding,
    hContentLength,
    hContentType,
    hUserAgent,
 )
import Network.HTTP.Types.Header (hAcceptEncoding, hVary)
import Network.Wai
import Network.Wai.Internal (Response (..))
import System.Directory (createDirectoryIfMissing, doesFileExist)
import qualified System.IO as IO

import Network.Wai.Header (contentLength)

data GzipSettings = GzipSettings
    { gzipFiles :: GzipFiles
    , gzipCheckMime :: S.ByteString -> Bool
    }

-- | Gzip behavior for files.
data GzipFiles
    = GzipIgnore -- ^ Do not compress file responses.
    | GzipCompress -- ^ Compress files. Note that this may counteract
                   -- zero-copy response optimizations on some
                   -- platforms.
    | GzipCacheFolder FilePath -- ^ Compress files, caching them in
                               -- some directory.
    | GzipPreCompressed GzipFiles -- ^ If we use compression then try to use the filename with ".gz"
                                  -- appended to it, if the file is missing then try next action
                                  --
                                  -- @since 3.0.17
    deriving (Show, Eq, Read)

-- | Use default MIME settings; /do not/ compress files.
instance Default GzipSettings where
    def = GzipSettings GzipIgnore defaultCheckMime

-- | MIME types that will be compressed by default:
-- @text/@ @*@, @application/json@, @application/javascript@,
-- @application/ecmascript@, @image/x-icon@.
defaultCheckMime :: S.ByteString -> Bool
defaultCheckMime bs =
    S8.isPrefixOf "text/" bs || bs' `Set.member` toCompress
  where
    bs' = fst $ S.break (== _semicolon) bs
    toCompress = Set.fromList
        [ "application/json"
        , "application/javascript"
        , "application/ecmascript"
        , "image/x-icon"
        ]

-- | Use gzip to compress the body of the response.
--
-- Analyzes the \"Accept-Encoding\" header from the client to determine
-- if gzip is supported.
--
-- File responses will be compressed according to the 'GzipFiles' setting.
--
-- Will only be applied based on the 'gzipCheckMime' setting. For default
-- behavior, see 'defaultCheckMime'.
gzip :: GzipSettings -> Middleware
gzip set app req sendResponse'
    | skipCompress = app req sendResponse
    | otherwise = app req . checkCompress $ \res ->
        let runAction x = case x of
                (ResponseRaw{}, _) -> sendResponse res
                -- Always skip if 'GzipIgnore'
                (ResponseFile {}, GzipIgnore) -> sendResponse res
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
                    compressFile s hs file cache sendResponse
                -- Use streaming logic
                _ -> compressE res sendResponse
        in runAction (res, gzipFiles set)
  where
    isCorrectMime =
        maybe False (gzipCheckMime set) . lookup hContentType
    sendResponse = sendResponse' . mapResponseHeaders (vary:)
    vary = (hVary, "Accept-Encoding")

    -- Can we skip from just looking at the 'Request'?
    skipCompress =
        not acceptsGZipEncoding || isMSIE6
      where
        reqHdrs = requestHeaders req
        acceptsGZipEncoding =
            maybe False (elem "gzip" . splitCommas) $ hAcceptEncoding `lookup` reqHdrs
        isMSIE6 =
            maybe False ("MSIE 6" `S.isInfixOf`) $ hUserAgent `lookup` reqHdrs

    -- Can we skip just by looking at the current 'Response'?
    checkCompress :: (Response -> IO ResponseReceived) -> Response -> IO ResponseReceived
    checkCompress f res =
        if isEncodedAlready || notBigEnough
            then sendResponse res
            else f res
      where
        resHdrs = responseHeaders res
        isEncodedAlready = isJust $ hContentEncoding `lookup` resHdrs
        notBigEnough =
            maybe
                False -- This could be a streaming case
                (< minimumLength)
                $ contentLength resHdrs

-- For a small enough response, gzipping will actually increase the size
-- Potentially for anything less than 860 bytes gzipping could be a net loss
-- The actual number is application specific though and may need to be adjusted
-- http://webmasters.stackexchange.com/questions/31750/what-is-recommended-minimum-object-size-for-gzip-performance-benefits
minimumLength :: Integer
minimumLength = 860

-- TODO: Add ETag functionality
compressFile :: Status -> [Header] -> FilePath -> FilePath -> (Response -> IO a) -> IO a
compressFile s hs file cache sendResponse = do
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
            either onErr (const onSucc) (x :: Either SomeException ()) -- FIXME bad! don't catch all exceptions like that!
  where
    onSucc = sendResponse $ responseFile s (fixHeaders hs) tmpfile Nothing

    onErr _ = sendResponse $ responseFile s hs file Nothing -- FIXME log the error message

    tmpfile = cache ++ '/' : map safe file
    safe c
        | 'A' <= c && c <= 'Z' = c
        | 'a' <= c && c <= 'z' = c
        | '0' <= c && c <= '9' = c
    safe '-' = '-'
    safe '_' = '_'
    safe _ = '_'

compressE :: Response
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
    ((hContentEncoding, "gzip") :) . filter notLength
  where
    notLength (x, _) = x /= hContentLength

splitCommas :: S.ByteString -> [S.ByteString]
splitCommas = map (S.dropWhile (== _space)) . S.split _comma
