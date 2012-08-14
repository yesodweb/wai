{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.Warp.Request where

import Control.Applicative
import Control.Exception.Lifted (throwIO)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B
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

-- FIXME come up with good values here
maxTotalHeaderLength :: Int
maxTotalHeaderLength = 50 * 1024

parseRequest :: Connection -> Port -> SockAddr
             -> Source (ResourceT IO) S.ByteString
             -> ResourceT IO (Request, IO (ResumableSource (ResourceT IO) ByteString))
parseRequest conn port remoteHost' src1 = do
    (src2, headers') <- src1 $$+ takeHeaders
    parseRequest' conn port headers' remoteHost' src2

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
              -> Port
              -> [ByteString]
              -> SockAddr
              -> ResumableSource (ResourceT IO) S.ByteString -- FIXME was buffered
              -> ResourceT IO (Request, IO (ResumableSource (ResourceT IO) ByteString))
parseRequest' _ _ [] _ _ = throwIO $ NotEnoughLines []
parseRequest' conn port (firstLine:otherLines) remoteHost' src = do
    (method, rpath', gets, httpversion) <- parseFirst firstLine
    let (host',rpath)
            | S.null rpath' = ("", "/")
            | "http://" `S.isPrefixOf` rpath' = S.breakByte 47 $ S.drop 7 rpath'
            | otherwise = ("", rpath')
    heads <- liftIO
           $ handleExpect conn httpversion id
             (map parseHeaderNoAttr otherLines)
    let host = fromMaybe host' $ lookup "host" heads
    let len0 =
            case lookup H.hContentLength heads of
                Nothing -> 0
                Just bs -> readInt bs
    let serverName' = takeUntil 58 host -- ':'
    let chunked = maybe False ((== "chunked") . CI.foldCase)
                  $ lookup "transfer-encoding" heads
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
            , requestBody = rbody
            , vault = mempty
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
                (hfirst, hsecond) = B.splitAt 5 http'
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

push :: THStatus -> ByteString -> Pipe ByteString ByteString Void () (ResourceT IO) [ByteString]
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
            in Just (nl, c == 32 || c == 9)
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
checkCR bs pos = if '\r' == B.index bs p then p else pos
  where
    !p = pos - 1
