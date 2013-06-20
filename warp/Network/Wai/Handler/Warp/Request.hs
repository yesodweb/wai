{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Request where

import Control.Applicative
import Control.Exception.Lifted (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B (unpack)
import qualified Data.ByteString.Unsafe as SU
import qualified Data.CaseInsensitive as CI
import Data.Conduit
import qualified Data.IORef as I
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Void (Void)
import Data.Word (Word8)
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Handler.Warp.Conduit
import Network.Wai.Handler.Warp.ReadInt
import Network.Wai.Handler.Warp.Types
import Prelude hiding (lines)
import qualified Network.Wai.Handler.Warp.Timeout as Timeout

-- FIXME come up with good values here
maxTotalHeaderLength :: Int
maxTotalHeaderLength = 50 * 1024

parseRequest :: Connection
             -> Port -> SockAddr
             -> Source (ResourceT IO) ByteString
             -> ResourceT IO (Request, IO (ResumableSource (ResourceT IO) ByteString))
parseRequest conn = parseRequestInternal conn Timeout.dummyHandle

parseRequestInternal
             :: Connection
             -> Timeout.Handle
             -> Port -> SockAddr
             -> Source (ResourceT IO) ByteString
             -> ResourceT IO (Request, IO (ResumableSource (ResourceT IO) ByteString))
parseRequestInternal conn timeoutHandle port remoteHost' src1 = do
    (src2, headers') <- src1 $$+ takeHeaders
    parseRequest' conn timeoutHandle port headers' remoteHost' src2

handleExpect :: Connection
             -> H.HttpVersion
             -> ([H.Header] -> [H.Header])
             -> [H.Header]
             -> IO [H.Header]
handleExpect _ _ front [] = return $ front []
handleExpect conn hv front (("expect", "100-continue"):rest) = do
    connSendAll conn $
        if hv == H.http11
            then "HTTP/1.1 100 Continue\r\n\r\n"
            else "HTTP/1.0 100 Continue\r\n\r\n"
    return $ front rest
handleExpect conn hv front (x:xs) = handleExpect conn hv (front . (x:)) xs

-- | Parse a set of header lines and body into a 'Request'.
parseRequest' :: Connection
              -> Timeout.Handle
              -> Port
              -> [ByteString]
              -> SockAddr
              -> ResumableSource (ResourceT IO) ByteString -- FIXME was buffered
              -> ResourceT IO (Request, IO (ResumableSource (ResourceT IO) ByteString))
parseRequest' _ _ _ [] _ _ = throwIO $ NotEnoughLines []
parseRequest' conn timeoutHandle port (firstLine:otherLines) remoteHost' src = do
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let (host',rpath)
            | S.null rpath' = ("", "/")
            | "http://" `S.isPrefixOf` rpath' = S.breakByte 47 $ S.drop 7 rpath'
            | otherwise = ("", rpath')
    heads <- liftIO
           $ handleExpect conn httpversion id
             (map parseHeaderNoAttr otherLines)
    let host = fromMaybe host' $ lookup hHost heads
    let len0 =
            case lookup H.hContentLength heads of
                Nothing -> 0
                Just bs -> readInt bs
    let serverName' = takeUntil 58 host -- ':'
    let chunked = maybe False ((== "chunked") . CI.foldCase)
                  $ lookup hTransferEncoding heads
    (rbody, getSource) <- liftIO $
        if chunked
          then do
            ref <- I.newIORef (src, NeedLen)
            return (chunkedSource ref, fst <$> I.readIORef ref)
          else do
            ibs <- IsolatedBSSource <$> I.newIORef (len0, src)
            return (ibsIsolate ibs, ibsDone ibs)

    return (Request
            { requestMethod = method
            , httpVersion = httpversion
            , pathInfo = H.decodePathSegments rpath
            , rawPathInfo = rpath
            , rawQueryString = gets
            , queryString = H.parseQuery gets
            , serverName = serverName'
            , serverPort = port
            , requestHeaders = heads
            , isSecure = False
            , remoteHost = remoteHost'
            , requestBody = do
                -- Timeout handling was paused after receiving the full request
                -- headers. Now we need to resume it to avoid a slowloris
                -- attack during request body sending.
                liftIO $ Timeout.resume timeoutHandle
                -- As soon as we finish receiving the request body, whether
                -- because the application is not interested in more bytes, or
                -- because there is no more data available, pause the timeout
                -- handler again.
                addCleanup (const $ liftIO $ Timeout.pause timeoutHandle) rbody
            , vault = mempty
#if MIN_VERSION_wai(1, 4, 0)
            , requestBodyLength =
                if chunked
                    then ChunkedBody
                    else KnownLength $ fromIntegral len0
#endif
            }, getSource)

{-# INLINE takeUntil #-}
takeUntil :: Word8 -> ByteString -> ByteString
takeUntil c bs =
    case S.elemIndex c bs of
       Just !idx -> SU.unsafeTake idx bs
       Nothing -> bs

{-# INLINE parseFirst #-} -- FIXME is this inline necessary? the function is only called from one place and not exported
parseFirst :: ByteString
           -> ResourceT IO (ByteString, ByteString, ByteString, H.HttpVersion)
parseFirst s =
    case filter (not . S.null) $ S.splitWith (\c -> c == 32 || c == 9) s of  -- ' '
        (method:query:http'') -> do
            let http' = S.concat http''
                (hfirst, hsecond) = S.splitAt 5 http'
            if hfirst == "HTTP/"
               then let (rpath, qstring) = S.breakByte 63 query  -- '?'
                        hv =
                            case hsecond of
                                "1.1" -> H.http11
                                _ -> H.http10
                    in return (method, rpath, qstring, hv)
               else throwIO NonHttp
        _ -> throwIO $ BadFirstLine $ B.unpack s

parseHeaderNoAttr :: ByteString -> H.Header
parseHeaderNoAttr s =
    let (k, rest) = S.breakByte 58 s -- ':'
        rest' = S.dropWhile (\c -> c == 32 || c == 9) $ S.drop 1 rest
     in (CI.mk k, rest')

type BSEndo = ByteString -> ByteString
type BSEndoList = [ByteString] -> [ByteString]

data THStatus = THStatus
    {-# UNPACK #-} !Int -- running total byte count
    BSEndoList -- previously parsed lines
    BSEndo -- bytestrings to be prepended

{-# INLINE takeHeaders #-}
takeHeaders :: Sink ByteString (ResourceT IO) [ByteString]
takeHeaders =
    await >>= maybe (throwIO ConnectionClosedByPeer) (push (THStatus 0 id id))

close :: Sink ByteString (ResourceT IO) a
close = throwIO IncompleteHeaders

push :: THStatus -> ByteString -> Sink ByteString (ResourceT IO) [ByteString]
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
            in Just (nl, (not b) && (c == 32 || c == 9))
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
