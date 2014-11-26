{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Network.Wai.Handler.Warp.Request (
    recvRequest
  , headerLines
  ) where

import qualified Control.Concurrent as Conc (yield)
import Control.Exception (throwIO)
import Data.Array ((!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import qualified Data.CaseInsensitive as CI
import qualified Data.IORef as I
import Data.Monoid (mempty)
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Handler.Warp.Conduit
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.ReadInt
import Network.Wai.Handler.Warp.RequestHeader
import Network.Wai.Handler.Warp.Settings (Settings, settingsNoParsePath)
import qualified Network.Wai.Handler.Warp.Timeout as Timeout
import Network.Wai.Handler.Warp.Types
import Network.Wai.Internal
import Prelude hiding (lines)
import Control.Monad (when)

----------------------------------------------------------------

-- FIXME come up with good values here
maxTotalHeaderLength :: Int
maxTotalHeaderLength = 50 * 1024

----------------------------------------------------------------

-- | Receiving a HTTP request from 'Connection' and parsing its header
--   to create 'Request'.
recvRequest :: Settings
            -> Connection
            -> InternalInfo
            -> SockAddr -- ^ Peer's address.
            -> Source -- ^ Where HTTP request comes from.
            -> IO (Request
                  ,Maybe (I.IORef Int)
                  ,IndexedHeader) -- ^
            -- 'Request' passed to 'Application',
            -- how many bytes remain to be consumed, if known
            -- 'IndexedHeader' of HTTP request for internal use,

recvRequest settings conn ii addr src = do
    hdrlines <- headerLines src
    (method, unparsedPath, path, query, httpversion, hdr) <- parseHeaderLines hdrlines
    let idxhdr = indexRequestHeader hdr
        expect = idxhdr ! idxExpect
        cl = idxhdr ! idxContentLength
        te = idxhdr ! idxTransferEncoding
    handleExpect conn httpversion expect
    (rbody, remainingRef, bodyLength) <- bodyAndSource src cl te
    rbody' <- timeoutBody th rbody
    let req = Request {
            requestMethod     = method
          , httpVersion       = httpversion
          , pathInfo          = H.decodePathSegments path
          , rawPathInfo       = if settingsNoParsePath settings then unparsedPath else path
          , rawQueryString    = query
          , queryString       = H.parseQuery query
          , requestHeaders    = hdr
          , isSecure          = False
          , remoteHost        = addr
          , requestBody       = rbody'
          , vault             = mempty
          , requestBodyLength = bodyLength
          , requestHeaderHost = idxhdr ! idxHost
          , requestHeaderRange = idxhdr ! idxRange
          }
    return (req, remainingRef, idxhdr)
  where
    th = threadHandle ii

----------------------------------------------------------------

headerLines :: Source -> IO [ByteString]
headerLines src = do
    bs <- readSource src
    if S.null bs
        then throwIO $ NotEnoughLines []
        else push src (THStatus 0 id id) bs

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

timeoutBody :: Timeout.Handle -> IO ByteString -> IO (IO ByteString)
timeoutBody timeoutHandle rbody = do
    isFirstRef <- I.newIORef True

    return $ do
        isFirst <- I.readIORef isFirstRef

        when isFirst $
            -- Timeout handling was paused after receiving the full request
            -- headers. Now we need to resume it to avoid a slowloris
            -- attack during request body sending.
            Timeout.resume timeoutHandle

        bs <- rbody

        -- As soon as we finish receiving the request body, whether
        -- because the application is not interested in more bytes, or
        -- because there is no more data available, pause the timeout
        -- handler again.
        when (S.null bs) (Timeout.pause timeoutHandle)

        return bs

----------------------------------------------------------------

type BSEndo = ByteString -> ByteString
type BSEndoList = [ByteString] -> [ByteString]

data THStatus = THStatus
    {-# UNPACK #-} !Int -- running total byte count
    BSEndoList -- previously parsed lines
    BSEndo -- bytestrings to be prepended

----------------------------------------------------------------

{- FIXME
close :: Sink ByteString IO a
close = throwIO IncompleteHeaders
-}

push :: Source -> THStatus -> ByteString -> IO [ByteString]
push src (THStatus len lines prepend) bs'
        -- Too many bytes
        | len > maxTotalHeaderLength = throwIO OverLargeHeader
        | otherwise = push' mnl
  where
    bs = prepend bs'
    bsLen = S.length bs
    mnl = do
        nl <- S.elemIndex 10 bs
        -- check if there are two more bytes in the bs
        -- if so, see if the second of those is a horizontal space
        if bsLen > nl + 1 then
            let c = S.index bs (nl + 1)
                b = case nl of
                      0 -> True
                      1 -> S.index bs 0 == 13
                      _ -> False
            in Just (nl, not b && (c == 32 || c == 9))
            else
            Just (nl, False)

    {-# INLINE push' #-}
    push' :: Maybe (Int, Bool) -> IO [ByteString]
    -- No newline find in this chunk.  Add it to the prepend,
    -- update the length, and continue processing.
    push' Nothing = do
        bst <- readSource' src
        when (S.null bst) $ throwIO IncompleteHeaders
        push src status bst
      where
        len' = len + bsLen
        prepend' = S.append bs
        status = THStatus len' lines prepend'
    -- Found a newline, but next line continues as a multiline header
    push' (Just (end, True)) = push src status rest
      where
        rest = S.drop (end + 1) bs
        prepend' = S.append (SU.unsafeTake (checkCR bs end) bs)
        len' = len + end
        status = THStatus len' lines prepend'
    -- Found a newline at position end.
    push' (Just (end, False))
      -- leftover
      | S.null line = do
            when (start < bsLen) $ leftoverSource src (SU.unsafeDrop start bs)
            return (lines [])
      -- more headers
      | otherwise   = let len' = len + start
                          lines' = lines . (line:)
                          status = THStatus len' lines' id
                      in if start < bsLen then
                             -- more bytes in this chunk, push again
                             let bs'' = SU.unsafeDrop start bs
                              in push src status bs''
                           else do
                             -- no more bytes in this chunk, ask for more
                             bst <- readSource' src
                             when (S.null bs) $ throwIO IncompleteHeaders
                             push src status bst
      where
        start = end + 1 -- start of next chunk
        line = SU.unsafeTake (checkCR bs end) bs

{-# INLINE checkCR #-}
checkCR :: ByteString -> Int -> Int
checkCR bs pos = if pos > 0 && 13 == S.index bs p then p else pos -- 13 is CR
  where
    !p = pos - 1
