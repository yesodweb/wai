{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Warp.Response (
    sendResponse,
    sanitizeHeaderValue, -- for testing
    warpVersion,
    hasBody,
    replaceHeader,
    addServer, -- testing
    addAltSvc,
) where

import Data.Array ((!))
import qualified Data.ByteString as S
import Data.ByteString.Builder (Builder, byteString)
import Data.ByteString.Builder.Extra (flush)
import Data.ByteString.Builder.HTTP.Chunked (
    chunkedTransferEncoding,
    chunkedTransferTerminator,
 )
import qualified Data.ByteString.Char8 as C8
import qualified Data.CaseInsensitive as CI
import Data.Function (on)
import Data.List (deleteBy)
import Data.Streaming.ByteString.Builder (
    newByteStringBuilderRecv,
    reuseBufferStrategy,
 )
import Data.Version (showVersion)
import Data.Word8 (_cr, _lf, _space, _tab)
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as H
import Network.Wai
import Network.Wai.Internal
import qualified Paths_warp
import qualified System.TimeManager as T
import qualified UnliftIO

import Network.Wai.Handler.Warp.Buffer (toBuilderBuffer)
import qualified Network.Wai.Handler.Warp.Date as D
import Network.Wai.Handler.Warp.File
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.IO (toBufIOWith)
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.ResponseHeader
import Network.Wai.Handler.Warp.Settings
import Network.Wai.Handler.Warp.Types

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

-- | Sending a HTTP response to 'Connection' according to 'Response'.
--
--   Applications/middlewares MUST provide a proper 'H.ResponseHeaders'.
--   so that inconsistency does not happen.
--   No header is deleted by this function.
--
--   Especially, Applications/middlewares MUST provide a proper
--   Content-Type. They MUST NOT provide
--   Content-Length, Content-Range, and Transfer-Encoding
--   because they are inserted, when necessary,
--   regardless they already exist.
--   This function does not insert Content-Encoding. It's middleware's
--   responsibility.
--
--   The Date and Server header is added if not exist
--   in HTTP response header.
--
--   There are three basic APIs to create 'Response':
--
--   ['responseBuilder' :: 'H.Status' -> 'H.ResponseHeaders' -> 'Builder' -> 'Response']
--     HTTP response body is created from 'Builder'.
--     Transfer-Encoding: chunked is used in HTTP/1.1.
--
--   ['responseStream' :: 'H.Status' -> 'H.ResponseHeaders' -> 'StreamingBody' -> 'Response']
--     HTTP response body is created from 'Builder'.
--     Transfer-Encoding: chunked is used in HTTP/1.1.
--
--   ['responseRaw' :: ('IO' 'ByteString' -> ('ByteString' -> 'IO' ()) -> 'IO' ()) -> 'Response' -> 'Response']
--     No header is added and no Transfer-Encoding: is applied.
--
--   ['responseFile' :: 'H.Status' -> 'H.ResponseHeaders' -> 'FilePath' -> 'Maybe' 'FilePart' -> 'Response']
--     HTTP response body is sent (by sendfile(), if possible) for GET method.
--     HTTP response body is not sent by HEAD method.
--     Content-Length and Content-Range are automatically
--     added into the HTTP response header if necessary.
--     If Content-Length and Content-Range exist in the HTTP response header,
--     they would cause inconsistency.
--     \"Accept-Ranges: bytes\" is also inserted.
--
--     Applications are categorized into simple and sophisticated.
--     Sophisticated applications should specify 'Just' to
--     'Maybe' 'FilePart'. They should treat the conditional request
--     by themselves. A proper 'Status' (200 or 206) must be provided.
--
--     Simple applications should specify 'Nothing' to
--     'Maybe' 'FilePart'. The size of the specified file is obtained
--     by disk access or from the file info cache.
--     If-Modified-Since, If-Unmodified-Since, If-Range and Range
--     are processed. Since a proper status is chosen, 'Status' is
--     ignored. Last-Modified is inserted.
sendResponse
    :: Settings
    -> Connection
    -> InternalInfo
    -> T.Handle
    -> Request
    -- ^ HTTP request.
    -> IndexedHeader
    -- ^ Indexed header of HTTP request.
    -> IO ByteString
    -- ^ source from client, for raw response
    -> Response
    -- ^ HTTP response including status code and response header.
    -> IO Bool
    -- ^ Returing True if the connection is persistent.
sendResponse settings conn ii th req reqidxhdr src response = do
    hs <- addConnection . addAltSvc settings <$> addServerAndDate hs0
    if hasBody s
        then do
            -- The response to HEAD does not have body.
            -- But to handle the conditional requests defined RFC 7232 and
            -- to generate appropriate content-length, content-range,
            -- and status, the response to HEAD is processed here.
            --
            -- See definition of rsp below for proper body stripping.
            (ms, mlen) <- sendRsp conn ii th ver s hs rspidxhdr maxRspBufSize method rsp
            case ms of
                Nothing -> return ()
                Just realStatus -> logger req realStatus mlen
            T.tickle th
            return ret
        else do
            _ <- sendRsp conn ii th ver s hs rspidxhdr maxRspBufSize method RspNoBody
            logger req s Nothing
            T.tickle th
            return isPersist
  where
    defServer = settingsServerName settings
    logger = settingsLogger settings
    maxRspBufSize = settingsMaxBuilderResponseBufferSize settings
    ver = httpVersion req
    s = responseStatus response
    hs0 = sanitizeHeaders $ responseHeaders response
    rspidxhdr = indexResponseHeader hs0
    addConnection hs = if (hasBody s && not ret) || (not (hasBody s) && not isPersist)
                       then (H.hConnection, "close") : hs
                       else hs
    getdate = getDate ii
    addServerAndDate = addDate getdate rspidxhdr . addServer defServer rspidxhdr
    (isPersist, isChunked0) = infoFromRequest req reqidxhdr
    isChunked = not isHead && isChunked0
    (isKeepAlive, needsChunked) = infoFromResponse rspidxhdr (isPersist, isChunked)
    method = requestMethod req
    isHead = method == H.methodHead
    rsp = case response of
        ResponseFile _ _ path mPart -> RspFile path mPart reqidxhdr (T.tickle th)
        ResponseBuilder _ _ b
            | isHead -> RspNoBody
            | otherwise -> RspBuilder b needsChunked
        ResponseStream _ _ fb
            | isHead -> RspNoBody
            | otherwise -> RspStream fb needsChunked
        ResponseRaw raw _ -> RspRaw raw src
    -- Make sure we don't hang on to 'response' (avoid space leak)
    !ret = case response of
        ResponseFile{} -> isPersist
        ResponseBuilder{} -> isKeepAlive
        ResponseStream{} -> isKeepAlive
        ResponseRaw{} -> False

----------------------------------------------------------------

sanitizeHeaders :: H.ResponseHeaders -> H.ResponseHeaders
sanitizeHeaders = map (sanitize <$>)
  where
    sanitize v
        | containsNewlines v = sanitizeHeaderValue v -- slow path
        | otherwise = v -- fast path

{-# INLINE containsNewlines #-}
containsNewlines :: ByteString -> Bool
containsNewlines = S.any (\w -> w == _cr || w == _lf)

{-# INLINE sanitizeHeaderValue #-}
sanitizeHeaderValue :: ByteString -> ByteString
sanitizeHeaderValue v = case C8.lines $ S.filter (/= _cr) v of
    [] -> ""
    x : xs -> C8.intercalate "\r\n" (x : mapMaybe addSpaceIfMissing xs)
  where
    addSpaceIfMissing line = case S.uncons line of
        Nothing -> Nothing
        Just (first, _)
            | first == _space || first == _tab -> Just line
            | otherwise -> Just $ _space `S.cons` line

----------------------------------------------------------------

data Rsp
    = RspNoBody
    | RspFile FilePath (Maybe FilePart) IndexedHeader (IO ())
    | RspBuilder Builder Bool
    | RspStream StreamingBody Bool
    | RspRaw (IO ByteString -> (ByteString -> IO ()) -> IO ()) (IO ByteString)

----------------------------------------------------------------

sendRsp
    :: Connection
    -> InternalInfo
    -> T.Handle
    -> H.HttpVersion
    -> H.Status
    -> H.ResponseHeaders
    -> IndexedHeader -- Response
    -> Int -- maxBuilderResponseBufferSize
    -> H.Method
    -> Rsp
    -> IO (Maybe H.Status, Maybe Integer)
----------------------------------------------------------------

sendRsp conn _ _ ver s hs _ _ _ RspNoBody = do
    -- Not adding Content-Length.
    -- User agents treats it as Content-Length: 0.
    composeHeader ver s hs >>= connSendAll conn
    return (Just s, Nothing)

----------------------------------------------------------------

sendRsp conn _ th ver s hs _ maxRspBufSize _ (RspBuilder body needsChunked) = do
    header <- composeHeaderBuilder ver s hs needsChunked
    let hdrBdy
            | needsChunked =
                header
                    <> chunkedTransferEncoding body
                    <> chunkedTransferTerminator
            | otherwise = header <> body
        writeBufferRef = connWriteBuffer conn
    len <-
        toBufIOWith
            maxRspBufSize
            writeBufferRef
            (\bs -> connSendAll conn bs >> T.tickle th)
            hdrBdy
    return (Just s, Just len)

----------------------------------------------------------------

sendRsp conn _ th ver s hs _ _ _ (RspStream streamingBody needsChunked) = do
    header <- composeHeaderBuilder ver s hs needsChunked
    (recv, finish) <-
        newByteStringBuilderRecv $
            reuseBufferStrategy $
                toBuilderBuffer $
                    connWriteBuffer conn
    let send builder = do
            popper <- recv builder
            let loop = do
                    bs <- popper
                    unless (S.null bs) $ do
                        sendFragment conn th bs
                        loop
            loop
        sendChunk
            | needsChunked = send . chunkedTransferEncoding
            | otherwise = send
    send header
    streamingBody sendChunk (sendChunk flush)
    when needsChunked $ send chunkedTransferTerminator
    mbs <- finish
    maybe (return ()) (sendFragment conn th) mbs
    return (Just s, Nothing) -- fixme: can we tell the actual sent bytes?

----------------------------------------------------------------

sendRsp conn _ th _ _ _ _ _ _ (RspRaw withApp src) = do
    withApp recv send
    return (Nothing, Nothing)
  where
    recv = do
        bs <- src
        unless (S.null bs) $ T.tickle th
        return bs
    send bs = connSendAll conn bs >> T.tickle th

----------------------------------------------------------------

-- Sophisticated WAI applications.
-- We respect s0. s0 MUST be a proper value.
sendRsp conn ii th ver s0 hs0 rspidxhdr maxRspBufSize method (RspFile path (Just part) _ hook) =
    sendRspFile2XX
        conn
        ii
        th
        ver
        s0
        hs
        rspidxhdr
        maxRspBufSize
        method
        path
        beg
        len
        hook
  where
    beg = filePartOffset part
    len = filePartByteCount part
    hs = addContentHeadersForFilePart hs0 part

----------------------------------------------------------------

-- Simple WAI applications.
-- Status is ignored
sendRsp conn ii th ver _ hs0 rspidxhdr maxRspBufSize method (RspFile path Nothing reqidxhdr hook) = do
    efinfo <- UnliftIO.tryIO $ getFileInfo ii path
    case efinfo of
        Left (_ex :: UnliftIO.IOException) ->
#ifdef WARP_DEBUG
            print _ex >>
#endif
            sendRspFile404 conn ii th ver hs0 rspidxhdr maxRspBufSize method
        Right finfo -> case conditionalRequest finfo hs0 method rspidxhdr reqidxhdr of
            WithoutBody s ->
                sendRsp conn ii th ver s hs0 rspidxhdr maxRspBufSize method RspNoBody
            WithBody s hs beg len ->
                sendRspFile2XX
                    conn
                    ii
                    th
                    ver
                    s
                    hs
                    rspidxhdr
                    maxRspBufSize
                    method
                    path
                    beg
                    len
                    hook

----------------------------------------------------------------

sendRspFile2XX
    :: Connection
    -> InternalInfo
    -> T.Handle
    -> H.HttpVersion
    -> H.Status
    -> H.ResponseHeaders
    -> IndexedHeader
    -> Int
    -> H.Method
    -> FilePath
    -> Integer
    -> Integer
    -> IO ()
    -> IO (Maybe H.Status, Maybe Integer)
sendRspFile2XX conn ii th ver s hs rspidxhdr maxRspBufSize method path beg len hook
    | method == H.methodHead =
        sendRsp conn ii th ver s hs rspidxhdr maxRspBufSize method RspNoBody
    | otherwise = do
        lheader <- composeHeader ver s hs
        (mfd, fresher) <- getFd ii path
        let fid = FileId path mfd
            hook' = hook >> fresher
        connSendFile conn fid beg len hook' [lheader]
        return (Just s, Just len)

sendRspFile404
    :: Connection
    -> InternalInfo
    -> T.Handle
    -> H.HttpVersion
    -> H.ResponseHeaders
    -> IndexedHeader
    -> Int
    -> H.Method
    -> IO (Maybe H.Status, Maybe Integer)
sendRspFile404 conn ii th ver hs0 rspidxhdr maxRspBufSize method =
    sendRsp
        conn
        ii
        th
        ver
        s
        hs
        rspidxhdr
        maxRspBufSize
        method
        (RspBuilder body True)
  where
    s = H.notFound404
    hs = replaceHeader H.hContentType "text/plain; charset=utf-8" hs0
    body = byteString "File not found"

----------------------------------------------------------------
----------------------------------------------------------------

-- | Use 'connSendAll' to send this data while respecting timeout rules.
sendFragment :: Connection -> T.Handle -> ByteString -> IO ()
sendFragment Connection{connSendAll = send} th bs = do
    T.resume th
    send bs
    T.pause th

-- We pause timeouts before passing control back to user code. This ensures
-- that a timeout will only ever be executed when Warp is in control. We
-- also make sure to resume the timeout after the completion of user code
-- so that we can kill idle connections.

----------------------------------------------------------------

infoFromRequest
    :: Request
    -> IndexedHeader
    -> ( Bool -- isPersist
       , Bool -- isChunked
       )
infoFromRequest req reqidxhdr = (checkPersist req reqidxhdr, checkChunk req)

checkPersist :: Request -> IndexedHeader -> Bool
checkPersist req reqidxhdr
    | ver == H.http11 = checkPersist11 conn
    | otherwise = checkPersist10 conn
  where
    ver = httpVersion req
    conn = reqidxhdr ! fromEnum ReqConnection
    checkPersist11 (Just x)
        | CI.foldCase x == "close" = False
    checkPersist11 _ = True
    checkPersist10 (Just x)
        | CI.foldCase x == "keep-alive" = True
    checkPersist10 _ = False

checkChunk :: Request -> Bool
checkChunk req = httpVersion req == H.http11

----------------------------------------------------------------

-- Used for ResponseBuilder and ResponseSource.
-- Don't use this for ResponseFile since this logic does not fit
-- for ResponseFile. For instance, isKeepAlive should be True in some cases
-- even if the response header does not have Content-Length.
--
-- Content-Length is specified by a reverse proxy.
-- Note that CGI does not specify Content-Length.
infoFromResponse :: IndexedHeader -> (Bool, Bool) -> (Bool, Bool)
infoFromResponse rspidxhdr (isPersist, isChunked) = (isKeepAlive, needsChunked)
  where
    needsChunked = isChunked && not hasLength
    isKeepAlive = isPersist && (isChunked || hasLength)
    hasLength = isJust $ rspidxhdr ! fromEnum ResContentLength

----------------------------------------------------------------

hasBody :: H.Status -> Bool
hasBody s =
    sc /= 204
        && sc /= 304
        && sc >= 200
  where
    sc = H.statusCode s

----------------------------------------------------------------

addTransferEncoding :: H.ResponseHeaders -> H.ResponseHeaders
addTransferEncoding hdrs = (H.hTransferEncoding, "chunked") : hdrs

addDate
    :: IO D.GMTDate -> IndexedHeader -> H.ResponseHeaders -> IO H.ResponseHeaders
addDate getdate rspidxhdr hdrs = case rspidxhdr ! fromEnum ResDate of
    Nothing -> do
        gmtdate <- getdate
        return $ (H.hDate, gmtdate) : hdrs
    Just _ -> return hdrs

----------------------------------------------------------------

-- | The version of Warp.
warpVersion :: String
warpVersion = showVersion Paths_warp.version

{-# INLINE addServer #-}
addServer
    :: HeaderValue -> IndexedHeader -> H.ResponseHeaders -> H.ResponseHeaders
addServer "" rspidxhdr hdrs = case rspidxhdr ! fromEnum ResServer of
    Nothing -> hdrs
    _ -> filter ((/= H.hServer) . fst) hdrs
addServer serverName rspidxhdr hdrs = case rspidxhdr ! fromEnum ResServer of
    Nothing -> (H.hServer, serverName) : hdrs
    _ -> hdrs

addAltSvc :: Settings -> H.ResponseHeaders -> H.ResponseHeaders
addAltSvc settings hs = case settingsAltSvc settings of
    Nothing -> hs
    Just v -> ("Alt-Svc", v) : hs

----------------------------------------------------------------

-- |
--
-- >>> replaceHeader "Content-Type" "new" [("content-type","old")]
-- [("Content-Type","new")]
replaceHeader
    :: H.HeaderName -> HeaderValue -> H.ResponseHeaders -> H.ResponseHeaders
replaceHeader k v hdrs = (k, v) : deleteBy ((==) `on` fst) (k, v) hdrs

----------------------------------------------------------------

composeHeaderBuilder
    :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> IO Builder
composeHeaderBuilder ver s hs True =
    byteString <$> composeHeader ver s (addTransferEncoding hs)
composeHeaderBuilder ver s hs False =
    byteString <$> composeHeader ver s hs
