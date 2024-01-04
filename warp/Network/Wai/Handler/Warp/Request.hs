{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Network.Wai.Handler.Warp.Request (
    recvRequest,
    headerLines,
    pauseTimeoutKey,
    getFileInfoKey,
#ifdef MIN_VERSION_crypton_x509
    getClientCertificateKey,
#endif
    NoKeepAliveRequest (..),
) where

import qualified Control.Concurrent as Conc (yield)
import Data.Array ((!))
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Unsafe as SU
import qualified Data.CaseInsensitive as CI
import qualified Data.IORef as I
import Data.Typeable (Typeable)
import qualified Data.Vault.Lazy as Vault
import Data.Word8 (_cr, _lf, _space, _tab)
#ifdef MIN_VERSION_crypton_x509
import Data.X509
#endif
import UnliftIO (Exception, throwIO)
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Handler.Warp.Types
import Network.Wai.Internal
import System.IO.Unsafe (unsafePerformIO)
import qualified System.TimeManager as Timeout
import Prelude hiding (lines)

import Network.Wai.Handler.Warp.Conduit
import Network.Wai.Handler.Warp.FileInfoCache
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.Imports hiding (readInt)
import Network.Wai.Handler.Warp.ReadInt
import Network.Wai.Handler.Warp.RequestHeader
import Network.Wai.Handler.Warp.Settings (
    Settings,
    settingsMaxTotalHeaderLength,
    settingsNoParsePath,
 )

----------------------------------------------------------------

-- | Receiving a HTTP request from 'Connection' and parsing its header
--   to create 'Request'.
recvRequest
    :: Bool
    -- ^ first request on this connection?
    -> Settings
    -> Connection
    -> InternalInfo
    -> Timeout.Handle
    -> SockAddr
    -- ^ Peer's address.
    -> Source
    -- ^ Where HTTP request comes from.
    -> Transport
    -> IO
        ( Request
        , Maybe (I.IORef Int)
        , IndexedHeader
        , IO ByteString
        )
    -- ^
    -- 'Request' passed to 'Application',
    -- how many bytes remain to be consumed, if known
    -- 'IndexedHeader' of HTTP request for internal use,
    -- Body producing action used for flushing the request body
recvRequest firstRequest settings conn ii th addr src transport = do
    hdrlines <- headerLines (settingsMaxTotalHeaderLength settings) firstRequest src
    (method, unparsedPath, path, query, httpversion, hdr) <-
        parseHeaderLines hdrlines
    let idxhdr = indexRequestHeader hdr
        expect = idxhdr ! fromEnum ReqExpect
        cl = idxhdr ! fromEnum ReqContentLength
        te = idxhdr ! fromEnum ReqTransferEncoding
        handle100Continue = handleExpect conn httpversion expect
        rawPath = if settingsNoParsePath settings then unparsedPath else path
        vaultValue =
            Vault.insert pauseTimeoutKey (Timeout.pause th)
                . Vault.insert getFileInfoKey (getFileInfo ii)
#ifdef MIN_VERSION_crypton_x509
                . Vault.insert getClientCertificateKey (getTransportClientCertificate transport)
#endif
                $ Vault.empty
    (rbody, remainingRef, bodyLength) <- bodyAndSource src cl te
    -- body producing function which will produce '100-continue', if needed
    rbody' <- timeoutBody remainingRef th rbody handle100Continue
    -- body producing function which will never produce 100-continue
    rbodyFlush <- timeoutBody remainingRef th rbody (return ())
    let req =
            Request
                { requestMethod = method
                , httpVersion = httpversion
                , pathInfo = H.decodePathSegments path
                , rawPathInfo = rawPath
                , rawQueryString = query
                , queryString = H.parseQuery query
                , requestHeaders = hdr
                , isSecure = isTransportSecure transport
                , remoteHost = addr
                , requestBody = rbody'
                , vault = vaultValue
                , requestBodyLength = bodyLength
                , requestHeaderHost = idxhdr ! fromEnum ReqHost
                , requestHeaderRange = idxhdr ! fromEnum ReqRange
                , requestHeaderReferer = idxhdr ! fromEnum ReqReferer
                , requestHeaderUserAgent = idxhdr ! fromEnum ReqUserAgent
                }
    return (req, remainingRef, idxhdr, rbodyFlush)

----------------------------------------------------------------

headerLines :: Int -> Bool -> Source -> IO [ByteString]
headerLines maxTotalHeaderLength firstRequest src = do
    bs <- readSource src
    if S.null bs
        then -- When we're working on a keep-alive connection and trying to
        -- get the second or later request, we don't want to treat the
        -- lack of data as a real exception. See the http1 function in
        -- the Run module for more details.

            if firstRequest
                then throwIO ConnectionClosedByPeer
                else throwIO NoKeepAliveRequest
        else push maxTotalHeaderLength src (THStatus 0 0 id id) bs

data NoKeepAliveRequest = NoKeepAliveRequest
    deriving (Show, Typeable)
instance Exception NoKeepAliveRequest

----------------------------------------------------------------

handleExpect
    :: Connection
    -> H.HttpVersion
    -> Maybe HeaderValue
    -> IO ()
handleExpect conn ver (Just "100-continue") = do
    connSendAll conn continue
    Conc.yield
  where
    continue
        | ver == H.http11 = "HTTP/1.1 100 Continue\r\n\r\n"
        | otherwise = "HTTP/1.0 100 Continue\r\n\r\n"
handleExpect _ _ _ = return ()

----------------------------------------------------------------

bodyAndSource
    :: Source
    -> Maybe HeaderValue
    -- ^ content length
    -> Maybe HeaderValue
    -- ^ transfer-encoding
    -> IO
        ( IO ByteString
        , Maybe (I.IORef Int)
        , RequestBodyLength
        )
bodyAndSource src cl te
    | chunked = do
        csrc <- mkCSource src
        return (readCSource csrc, Nothing, ChunkedBody)
    | otherwise = do
        isrc@(ISource _ remaining) <- mkISource src len
        return (readISource isrc, Just remaining, bodyLen)
  where
    len = toLength cl
    bodyLen = KnownLength $ fromIntegral len
    chunked = isChunked te

toLength :: Maybe HeaderValue -> Int
toLength Nothing = 0
toLength (Just bs) = readInt bs

isChunked :: Maybe HeaderValue -> Bool
isChunked (Just bs) = CI.foldCase bs == "chunked"
isChunked _ = False

----------------------------------------------------------------

timeoutBody
    :: Maybe (I.IORef Int)
    -- ^ remaining
    -> Timeout.Handle
    -> IO ByteString
    -> IO ()
    -> IO (IO ByteString)
timeoutBody remainingRef timeoutHandle rbody handle100Continue = do
    isFirstRef <- I.newIORef True

    let checkEmpty =
            case remainingRef of
                Nothing -> return . S.null
                Just ref -> \bs ->
                    if S.null bs
                        then return True
                        else do
                            x <- I.readIORef ref
                            return $! x <= 0

    return $ do
        isFirst <- I.readIORef isFirstRef

        when isFirst $ do
            -- Only check if we need to produce the 100 Continue status
            -- when asking for the first chunk of the body
            handle100Continue
            -- Timeout handling was paused after receiving the full request
            -- headers. Now we need to resume it to avoid a slowloris
            -- attack during request body sending.
            Timeout.resume timeoutHandle
            I.writeIORef isFirstRef False

        bs <- rbody

        -- As soon as we finish receiving the request body, whether
        -- because the application is not interested in more bytes, or
        -- because there is no more data available, pause the timeout
        -- handler again.
        isEmpty <- checkEmpty bs
        when isEmpty (Timeout.pause timeoutHandle)

        return bs

----------------------------------------------------------------

type BSEndo = B.Builder -> B.Builder
type BSEndoList = [ByteString] -> [ByteString]

data THStatus
    = THStatus
        !Int -- running total byte count (excluding current header chunk)
        !Int -- current header chunk byte count
        BSEndoList -- previously parsed lines
        BSEndo -- bytestrings to be prepended

----------------------------------------------------------------

{- FIXME
close :: Sink ByteString IO a
close = throwIO IncompleteHeaders
-}

-- | Assumes the 'ByteString' is never 'S.null'
push :: Int -> Source -> THStatus -> ByteString -> IO [ByteString]
push maxTotalHeaderLength src (THStatus totalLen chunkLen reqLines prepend) bs
    -- Too many bytes
    | currentTotal > maxTotalHeaderLength = throwIO OverLargeHeader
    | otherwise =
        case S.elemIndex _lf bs of
            -- No newline found
            Nothing -> withNewChunk noNewlineFound
            -- Newline found at index 'ix'
            Just ix -> newlineFound ix
  where
    bsLen = S.length bs
    currentTotal = totalLen + chunkLen
    isWS c = c == _space || c == _tab
    {-# INLINE finishUp #-}
    finishUp rest = do
        when (not $ S.null rest) $ leftoverSource src rest
        pure $ reqLines []
    {-# INLINE withNewChunk #-}
    withNewChunk :: (S.ByteString -> IO a) -> IO a
    withNewChunk f = do
        newChunk <- readSource' src
        when (S.null newChunk) $ throwIO IncompleteHeaders
        f newChunk
    {-# INLINE noNewlineFound #-}
    noNewlineFound newChunk
        -- The chunk split the CRLF in half
        | SU.unsafeLast bs == _cr && S.head newChunk == _lf =
            let bs' = SU.unsafeDrop 1 newChunk
             in if bsLen == 1 && chunkLen == 0
                -- first part is only CRLF, we're done
                then finishUp bs'
                else do
                    rest <- if S.length newChunk == 1
                        -- new chunk is only LF, we need more to check for multiline
                        then withNewChunk pure
                        else pure bs'
                    let nextIsWS = isWS $ SU.unsafeHead rest
                        status = addEither nextIsWS (bsLen + 1) (SU.unsafeTake (bsLen - 1) bs)
                    push maxTotalHeaderLength src status rest
        -- chunk and keep going
        | otherwise = push maxTotalHeaderLength src (addChunk bsLen bs) newChunk
    {-# INLINE newlineFound #-}
    newlineFound ix
        -- Is end of headers
        | startsWithLF && chunkLen == 0 =
            finishUp (SU.unsafeDrop end bs)
        | otherwise = do
            -- LF is on last byte
            rest <- if end == bsLen
                -- we need more chunks to check for whitespace
                then withNewChunk pure
                else pure $ SU.unsafeDrop end bs
            let nextIsWS = isWS $ SU.unsafeHead rest
                status = addEither nextIsWS end (SU.unsafeTake (checkCR bs ix) bs)
            push maxTotalHeaderLength src status rest
      where
        end = ix + 1
        startsWithLF =
            case ix of
                0 -> True
                1 -> SU.unsafeHead bs == _cr
                _ -> False
    addEither p =
        if p then addChunk else addLine
    -- addLine: take the current chunk and add to 'lines' as optimal as possible
    addLine len chunk =
        let newTotal = currentTotal + len
            newLine =
                if chunkLen == 0
                    then chunk
                    else S.toStrict $ B.toLazyByteString $ prepend $ B.byteString chunk
         in THStatus newTotal 0 (reqLines . (newLine:)) id
    -- addChunk: take the current chunk and add to 'prepend' as optimal as possible
    addChunk len chunk =
        let newChunkTotal = chunkLen + len
            newPrepend = prepend . (B.byteString chunk <>)
         in THStatus totalLen newChunkTotal reqLines newPrepend
{- HLint ignore push "Use unless" -}

{-# INLINE checkCR #-}
checkCR :: ByteString -> Int -> Int
checkCR bs pos
    | pos > 0 && S.index bs p == _cr = p
    | otherwise = pos
  where
    !p = pos - 1

pauseTimeoutKey :: Vault.Key (IO ())
pauseTimeoutKey = unsafePerformIO Vault.newKey
{-# NOINLINE pauseTimeoutKey #-}

getFileInfoKey :: Vault.Key (FilePath -> IO FileInfo)
getFileInfoKey = unsafePerformIO Vault.newKey
{-# NOINLINE getFileInfoKey #-}

#ifdef MIN_VERSION_crypton_x509
getClientCertificateKey :: Vault.Key (Maybe CertificateChain)
getClientCertificateKey = unsafePerformIO Vault.newKey
{-# NOINLINE getClientCertificateKey #-}
#endif
