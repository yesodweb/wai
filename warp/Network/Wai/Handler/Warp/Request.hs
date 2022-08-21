{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Network.Wai.Handler.Warp.Request (
    recvRequest
  , headerLines
  , pauseTimeoutKey
  , getFileInfoKey
#ifdef MIN_VERSION_x509
  , getClientCertificateKey
#endif
  , NoKeepAliveRequest (..)
  ) where

import qualified Control.Concurrent as Conc (yield)
import UnliftIO (throwIO, Exception)
import Data.Array ((!))
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import qualified Data.CaseInsensitive as CI
import qualified Data.IORef as I
import Data.Typeable (Typeable)
import qualified Data.Vault.Lazy as Vault
#ifdef MIN_VERSION_x509
import Data.X509
#endif
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Handler.Warp.Types
import Network.Wai.Internal
import Prelude hiding (lines)
import System.IO.Unsafe (unsafePerformIO)
import qualified System.TimeManager as Timeout

import Network.Wai.Handler.Warp.Conduit
import Network.Wai.Handler.Warp.FileInfoCache
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.Imports hiding (readInt)
import Network.Wai.Handler.Warp.ReadInt
import Network.Wai.Handler.Warp.RequestHeader
import Network.Wai.Handler.Warp.Settings (Settings, settingsNoParsePath, settingsMaxTotalHeaderLength)

----------------------------------------------------------------

-- | Receiving a HTTP request from 'Connection' and parsing its header
--   to create 'Request'.
recvRequest :: Bool -- ^ first request on this connection?
            -> Settings
            -> Connection
            -> InternalInfo
            -> Timeout.Handle
            -> SockAddr -- ^ Peer's address.
            -> Source -- ^ Where HTTP request comes from.
            -> Transport
            -> IO (Request
                  ,Maybe (I.IORef Int)
                  ,IndexedHeader
                  ,IO ByteString) -- ^
            -- 'Request' passed to 'Application',
            -- how many bytes remain to be consumed, if known
            -- 'IndexedHeader' of HTTP request for internal use,
            -- Body producing action used for flushing the request body

recvRequest firstRequest settings conn ii th addr src transport = do
    hdrlines <- headerLines (settingsMaxTotalHeaderLength settings) firstRequest src
    (method, unparsedPath, path, query, httpversion, hdr) <- parseHeaderLines hdrlines
    let idxhdr = indexRequestHeader hdr
        expect = idxhdr ! fromEnum ReqExpect
        cl = idxhdr ! fromEnum ReqContentLength
        te = idxhdr ! fromEnum ReqTransferEncoding
        handle100Continue = handleExpect conn httpversion expect
        rawPath = if settingsNoParsePath settings then unparsedPath else path
        vaultValue = Vault.insert pauseTimeoutKey (Timeout.pause th)
                   $ Vault.insert getFileInfoKey (getFileInfo ii)
#ifdef MIN_VERSION_x509
                   $ Vault.insert getClientCertificateKey (getTransportClientCertificate transport)
#endif
                     Vault.empty
    (rbody, remainingRef, bodyLength) <- bodyAndSource src cl te
    -- body producing function which will produce '100-continue', if needed
    rbody' <- timeoutBody remainingRef th rbody handle100Continue
    -- body producing function which will never produce 100-continue
    rbodyFlush <- timeoutBody remainingRef th rbody (return ())
    let req = Request {
            requestMethod     = method
          , httpVersion       = httpversion
          , pathInfo          = H.decodePathSegments path
          , rawPathInfo       = rawPath
          , rawQueryString    = query
          , queryString       = H.parseQuery query
          , requestHeaders    = hdr
          , isSecure          = isTransportSecure transport
          , remoteHost        = addr
          , requestBody       = rbody'
          , vault             = vaultValue
          , requestBodyLength = bodyLength
          , requestHeaderHost      = idxhdr ! fromEnum ReqHost
          , requestHeaderRange     = idxhdr ! fromEnum ReqRange
          , requestHeaderReferer   = idxhdr ! fromEnum ReqReferer
          , requestHeaderUserAgent = idxhdr ! fromEnum ReqUserAgent
          }
    return (req, remainingRef, idxhdr, rbodyFlush)

----------------------------------------------------------------

headerLines :: Int -> Bool -> Source -> IO [ByteString]
headerLines maxTotalHeaderLength firstRequest src = do
    bs <- readSource src
    if S.null bs
        -- When we're working on a keep-alive connection and trying to
        -- get the second or later request, we don't want to treat the
        -- lack of data as a real exception. See the http1 function in
        -- the Run module for more details.
        then if firstRequest then throwIO ConnectionClosedByPeer else throwIO NoKeepAliveRequest
        else push maxTotalHeaderLength src (THStatus 0 0 id id) bs

data NoKeepAliveRequest = NoKeepAliveRequest
    deriving (Show, Typeable)
instance Exception NoKeepAliveRequest

----------------------------------------------------------------

handleExpect :: Connection
             -> H.HttpVersion
             -> Maybe HeaderValue
             -> IO ()
handleExpect conn ver (Just "100-continue") = do
    connSendAll conn continue
    Conc.yield
  where
    continue
      | ver == H.http11 = "HTTP/1.1 100 Continue\r\n\r\n"
      | otherwise       = "HTTP/1.0 100 Continue\r\n\r\n"
handleExpect _    _   _                     = return ()

----------------------------------------------------------------

bodyAndSource :: Source
              -> Maybe HeaderValue -- ^ content length
              -> Maybe HeaderValue -- ^ transfer-encoding
              -> IO (IO ByteString
                    ,Maybe (I.IORef Int)
                    ,RequestBodyLength
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
toLength Nothing   = 0
toLength (Just bs) = readInt bs

isChunked :: Maybe HeaderValue -> Bool
isChunked (Just bs) = CI.foldCase bs == "chunked"
isChunked _         = False

----------------------------------------------------------------

timeoutBody :: Maybe (I.IORef Int) -- ^ remaining
            -> Timeout.Handle
            -> IO ByteString
            -> IO ()
            -> IO (IO ByteString)
timeoutBody remainingRef timeoutHandle rbody handle100Continue = do
    isFirstRef <- I.newIORef True

    let checkEmpty =
            case remainingRef of
                Nothing -> return . S.null
                Just ref -> \bs -> if S.null bs
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

type BSEndo = ByteString -> ByteString
type BSEndoList = [ByteString] -> [ByteString]

data THStatus = THStatus
    !Int -- running total byte count (excluding current header chunk)
    !Int -- current header chunk byte count
    BSEndoList -- previously parsed lines
    BSEndo -- bytestrings to be prepended

----------------------------------------------------------------

{- FIXME
close :: Sink ByteString IO a
close = throwIO IncompleteHeaders
-}

push :: Int -> Source -> THStatus -> ByteString -> IO [ByteString]
push maxTotalHeaderLength src (THStatus totalLen chunkLen lines prepend) bs'
        -- Too many bytes
        | currentTotal > maxTotalHeaderLength = throwIO OverLargeHeader
        | otherwise = push' mNL
  where
    currentTotal = totalLen + chunkLen
    -- bs: current header chunk, plus maybe (parts of) next header
    bs = prepend bs'
    bsLen = S.length bs
    -- Maybe newline
    -- Returns: Maybe
    --    ( length of this chunk up to newline
    --    , position of newline in relation to entire current header
    --    , is this part of a multiline header
    --    )
    mNL = do
        -- 10 is the code point for newline (\n)
        chunkNL <- S.elemIndex 10 bs'
        let headerNL = chunkNL + S.length (prepend "")
            chunkNLlen = chunkNL + 1
        -- check if there are two more bytes in the bs
        -- if so, see if the second of those is a horizontal space
        if bsLen > headerNL + 1 then
            let c = S.index bs (headerNL + 1)
                b = case headerNL of
                      0 -> True
                      1 -> S.index bs 0 == 13
                      _ -> False
                isMultiline = not b && (c == 32 || c == 9)
            in Just (chunkNLlen, headerNL, isMultiline)
            else
            Just (chunkNLlen, headerNL, False)

    {-# INLINE push' #-}
    push' :: Maybe (Int, Int, Bool) -> IO [ByteString]
    -- No newline find in this chunk.  Add it to the prepend,
    -- update the length, and continue processing.
    push' Nothing = do
        bst <- readSource' src
        when (S.null bst) $ throwIO IncompleteHeaders
        push maxTotalHeaderLength src status bst
      where
        prepend' = S.append bs
        thisChunkLen = S.length bs'
        newChunkLen = chunkLen + thisChunkLen
        status = THStatus totalLen newChunkLen lines prepend'
    -- Found a newline, but next line continues as a multiline header
    push' (Just (chunkNLlen, end, True)) =
        push maxTotalHeaderLength src status rest
      where
        rest = S.drop (end + 1) bs
        prepend' = S.append (SU.unsafeTake (checkCR bs end) bs)
        -- If we'd just update the entire current chunk up to newline
        -- we wouldn't count all the dropped newlines in between.
        -- So update 'chunkLen' with current chunk up to newline
        -- and use 'chunkLen' later on to add to 'totalLen'.
        newChunkLen = chunkLen + chunkNLlen
        status = THStatus totalLen newChunkLen lines prepend'
    -- Found a newline at position end.
    push' (Just (chunkNLlen, end, False))
      -- leftover
      | S.null line = do
            when (start < bsLen) $ leftoverSource src (SU.unsafeDrop start bs)
            return (lines [])
      -- more headers
      | otherwise   = let lines' = lines . (line:)
                          newTotalLength = totalLen + chunkLen + chunkNLlen
                          status = THStatus newTotalLength 0 lines' id
                      in if start < bsLen then
                             -- more bytes in this chunk, push again
                             let bs'' = SU.unsafeDrop start bs
                              in push maxTotalHeaderLength src status bs''
                           else do
                             -- no more bytes in this chunk, ask for more
                             bst <- readSource' src
                             when (S.null bs) $ throwIO IncompleteHeaders
                             push maxTotalHeaderLength src status bst
      where
        start = end + 1 -- start of next chunk
        line = SU.unsafeTake (checkCR bs end) bs

{-# INLINE checkCR #-}
checkCR :: ByteString -> Int -> Int
checkCR bs pos = if pos > 0 && 13 == S.index bs p then p else pos -- 13 is CR (\r)
  where
    !p = pos - 1

pauseTimeoutKey :: Vault.Key (IO ())
pauseTimeoutKey = unsafePerformIO Vault.newKey
{-# NOINLINE pauseTimeoutKey #-}

getFileInfoKey :: Vault.Key (FilePath -> IO FileInfo)
getFileInfoKey = unsafePerformIO Vault.newKey
{-# NOINLINE getFileInfoKey #-}

#ifdef MIN_VERSION_x509
getClientCertificateKey :: Vault.Key (Maybe CertificateChain)
getClientCertificateKey = unsafePerformIO Vault.newKey
{-# NOINLINE getClientCertificateKey #-}
#endif
