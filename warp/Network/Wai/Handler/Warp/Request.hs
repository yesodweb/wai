{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Request (
    recvRequest
  , headerLines
  ) where

import Control.Applicative
import qualified Control.Concurrent as Conc (yield)
import Control.Exception.Lifted (throwIO)
import Control.Monad.IO.Class (liftIO)
import Data.Array ((!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Unsafe as SU
import qualified Data.CaseInsensitive as CI
import Data.Conduit
import qualified Data.IORef as I
import Data.Monoid (mempty)
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Handler.Warp.Conduit
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.ReadInt
import Network.Wai.Handler.Warp.RequestHeader
import qualified Network.Wai.Handler.Warp.Timeout as Timeout
import Network.Wai.Handler.Warp.Types
import Network.Wai.Internal
import Prelude hiding (lines)

----------------------------------------------------------------

-- FIXME come up with good values here
maxTotalHeaderLength :: Int
maxTotalHeaderLength = 50 * 1024

----------------------------------------------------------------

recvRequest :: Connection
            -> Timeout.Handle
            -> SockAddr -- ^ Peer's address.
            -> Source IO ByteString -- ^ Where HTTP request comes from.
            -> IO (Request
                  ,IndexedHeader
                  ,IO (ResumableSource IO ByteString)) -- ^
            -- 'Request' passed to 'Application',
            -- 'IndexedHeader' of HTTP request for internal use, and
            -- leftover source (i.e. HTTP pipelining).

recvRequest conn timeoutHandle addr src0 = do
    (src, hdrlines) <- src0 $$+ headerLines
    (method, rpath, gets, httpversion, hdr) <- parseHeaderLines hdrlines
    let idxhdr = indexRequestHeader hdr
        expect = idxhdr ! idxExpect
        cl = idxhdr ! idxContentLength
        te = idxhdr ! idxTransferEncoding
    liftIO $ handleExpect conn httpversion expect
    (rbody, bodyLength, getSource) <- bodyAndSource src cl te
    let req = Request {
            requestMethod     = method
          , httpVersion       = httpversion
          , pathInfo          = H.decodePathSegments rpath
          , rawPathInfo       = rpath
          , rawQueryString    = gets
          , queryString       = H.parseQuery gets
          , requestHeaders    = hdr
          , isSecure          = False
          , remoteHost        = addr
          , requestBody       = timeoutBody timeoutHandle rbody
          , vault             = mempty
          , requestBodyLength = bodyLength
          }
    return (req, idxhdr, getSource)

----------------------------------------------------------------

headerLines :: Sink ByteString IO [ByteString]
headerLines =
    await >>= maybe (throwIO ConnectionClosedByPeer) (push (THStatus 0 id id))

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

bodyAndSource :: ResumableSource IO ByteString
              -> Maybe HeaderValue
              -> Maybe HeaderValue
              -> IO (Source IO ByteString
                    ,RequestBodyLength
                    ,IO (ResumableSource IO ByteString))
bodyAndSource src cl te
  | chunked = do
      ref <- I.newIORef (src, NeedLen)
      return (chunkedSource ref, ChunkedBody, fst <$> I.readIORef ref)
  | otherwise = do
      ibs <- IsolatedBSSource <$> I.newIORef (len, src)
      return (ibsIsolate ibs, bodyLen, ibsDone ibs)
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

timeoutBody :: Timeout.Handle -> Source IO ByteString -> Source IO ByteString
timeoutBody timeoutHandle rbody = do
    -- Timeout handling was paused after receiving the full request
    -- headers. Now we need to resume it to avoid a slowloris
    -- attack during request body sending.
    liftIO $ Timeout.resume timeoutHandle
    -- As soon as we finish receiving the request body, whether
    -- because the application is not interested in more bytes, or
    -- because there is no more data available, pause the timeout
    -- handler again.
    addCleanup (const $ liftIO $ Timeout.pause timeoutHandle) rbody

----------------------------------------------------------------

type BSEndo = ByteString -> ByteString
type BSEndoList = [ByteString] -> [ByteString]

data THStatus = THStatus
    {-# UNPACK #-} !Int -- running total byte count
    BSEndoList -- previously parsed lines
    BSEndo -- bytestrings to be prepended

----------------------------------------------------------------

close :: Sink ByteString IO a
close = throwIO IncompleteHeaders

push :: THStatus -> ByteString -> Sink ByteString IO [ByteString]
push (THStatus len lines prepend) bs
        -- Too many bytes
        | len > maxTotalHeaderLength = throwIO OverLargeHeader
        | otherwise = push' mnl
  where
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
    -- No newline find in this chunk.  Add it to the prepend,
    -- update the length, and continue processing.
    push' Nothing = await >>= maybe close (push status)
      where
        len' = len + bsLen
        prepend' = prepend . S.append bs
        status = THStatus len' lines prepend'
    -- Found a newline, but next line continues as a multiline header
    push' (Just (end, True)) = push status rest
      where
        rest = S.drop (end + 1) bs
        prepend' = prepend . S.append (SU.unsafeTake (checkCR bs end) bs)
        len' = len + end
        status = THStatus len' lines prepend'
    -- Found a newline at position end.
    push' (Just (end, False))
      -- leftover
      | S.null line = let lines' = lines []
                          rest = if start < bsLen then
                                     Just (SU.unsafeDrop start bs)
                                   else
                                     Nothing
                       in maybe (return ()) leftover rest >> return lines'
      -- more headers
      | otherwise   = let len' = len + start
                          lines' = lines . (line:)
                          status = THStatus len' lines' id
                      in if start < bsLen then
                             -- more bytes in this chunk, push again
                             let bs' = SU.unsafeDrop start bs
                              in push status bs'
                           else
                             -- no more bytes in this chunk, ask for more
                             await >>= maybe close (push status)
      where
        start = end + 1 -- start of next chunk
        line
          -- There were some bytes before the newline, get them
          | end > 0 = prepend $ SU.unsafeTake (checkCR bs end) bs
          -- No bytes before the newline
          | otherwise = prepend S.empty

{-# INLINE checkCR #-}
checkCR :: ByteString -> Int -> Int
checkCR bs pos = if 13 == S.index bs p then p else pos -- 13 is CR
  where
    !p = pos - 1
