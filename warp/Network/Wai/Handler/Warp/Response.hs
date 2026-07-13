{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Warp.Response (
    sendResponse,
    sanitizeHeaderValue, -- for testing
    --  Provided here for backwards compatibility.
    warpVersion,
    hasBody,
    replaceHeader,
    addServer, -- testing
    addAltSvc,
) where

import qualified Control.Exception as E
import Data.Array ((!))
import qualified Data.ByteString as S
import Data.ByteString.Internal (toForeignPtr, unsafeCreate)
import Data.ByteString.Builder (Builder, byteString)
import Data.ByteString.Builder.Extra (flush)
import Data.ByteString.Builder.HTTP.Chunked (
    chunkedTransferEncoding,
    chunkedTransferTerminator,
 )
import qualified Data.CaseInsensitive as CI
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef)
import Data.Streaming.ByteString.Builder (
    newByteStringBuilderRecv,
    reuseBufferStrategy,
 )
import Data.Word8 (_cr, _lf, _nul, _space)
import Foreign (copyBytes, plusForeignPtr, pokeByteOff, withForeignPtr)
import qualified Network.HTTP.Types as H
import qualified Network.HTTP.Types.Header as Header
import Network.Wai
import Network.Wai.Internal
import qualified System.TimeManager as T

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
    -- Decide connection persistence
    isShuttingDown <-
        case settingsServerState settings of
            Just serverState -> currentShuttingDownState serverState
            -- Should never be reached!
            -- (cf. 'makeServerState' in 'runSettingsConnectionMakerSecure')
            Nothing -> pure False
    let shouldPersist = not isShuttingDown && ret
        addConnection hs =
            if shouldPersist || responseWantsToClose
                then hs
                else (Header.hConnection, "close") : hs

    -- Adjust headers
    hs <- addConnection . addAltSvc settings <$> addServerAndDate hs0

    -- Start response logic
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
        else do
            _ <- sendRsp conn ii th ver s hs rspidxhdr maxRspBufSize method RspNoBody
            logger req s Nothing
    T.tickle th
    return shouldPersist
  where
    -- From Settings --
    defServer = settingsServerName settings
    logger = settingsLogger settings
    maxRspBufSize = settingsMaxBuilderResponseBufferSize settings

    -- From Request --
    method = requestMethod req
    isHead = method == H.methodHead
    ver = httpVersion req
    isHttp11 = ver == H.http11
    reqSaysPersist = checkReqConnectionHeader isHttp11 reqidxhdr

    -- From Response --
    s = responseStatus response
    hs0 = sanitizeHeaders $ responseHeaders response
    rspidxhdr = indexResponseHeader hs0
    hasLength = isJust $ rspidxhdr ! fromEnum ResContentLength
    responseWantsToClose =
        case rspidxhdr ! fromEnum ResConnection of
            Nothing -> False
            Just v -> CI.foldCase v == "close"
    isPersist = reqSaysPersist && not responseWantsToClose

    -- Other --
    getdate = getDate ii
    addServerAndDate = addDate getdate rspidxhdr . addServer defServer rspidxhdr
    needsChunked = isHttp11 && not hasLength
    rsp = case response of
        ResponseFile _ _ path mPart -> RspFile path mPart reqidxhdr (T.tickle th)
        ResponseBuilder _ _ b
            | isHead -> RspNoBody
            | otherwise -> RspBuilder b needsChunked
        ResponseStream _ _ fb
            | isHead -> RspNoBody
            | otherwise -> RspStream fb needsChunked
        ResponseRaw raw _ -> RspRaw raw src
    -- Should be False if (http10 && not hasLength), regardless of what
    -- the 'Connection' header says. (as long as the response should have a body)
    isKeepAlive =
        isPersist && (isHttp11 || hasLength || isHead || not (hasBody s))
    -- Make sure we don't hang on to 'response' (avoid space leak)
    !ret = case response of
        -- Will get 'Content-Length' header later on using the
        -- 'addContentHeaders(ForFilePart)' functions, so if the
        -- 'Connection' header says we persist, we persist.
        ResponseFile{} -> isPersist
        ResponseBuilder{} -> isKeepAlive
        ResponseStream{} -> isKeepAlive
        -- Is already an ongoing open connection, so if it is done,
        -- the connection should be closed.
        ResponseRaw{} -> False

----------------------------------------------------------------

-- | As per RFC 9110 we replace any newlines (\r\n) or \NUL with spaces
sanitizeHeaders :: H.ResponseHeaders -> H.ResponseHeaders
sanitizeHeaders = map (sanitizeHeaderValue <$>)

{-# INLINE isRecoverableWhitespace #-}
-- | CR, LF and NUL can safely be replaced with a SP according to RFC 9110
-- <https://www.rfc-editor.org/rfc/rfc9110.html#section-5.5-5>
isRecoverableWhitespace :: Word8 -> Bool
isRecoverableWhitespace w = w == _cr || w == _lf || w == _nul

sanitizeHeaderValue :: ByteString -> ByteString
sanitizeHeaderValue v =
    case S.findIndices isRecoverableWhitespace v of
        -- Nothing to replace
        [] -> v
        -- Found CR, LF or NUL.
        ixs ->
            unsafeCreate len $ \dst -> do
                withForeignPtr fptr $ \src -> do
                    -- copy the bytestring
                    copyBytes dst src len
                    -- and then replace the offending bytes
                    for_ ixs $ \ix -> pokeByteOff dst ix _space
  where
    (fptr', offset, len) = toForeignPtr v
    -- We need to use the offset for backwards compatibility with
    -- "bytestring < 0.11"
    fptr = fptr' `plusForeignPtr` offset

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

sendRsp conn _ th ver s hs rspidxhdr maxRspBufSize _ (RspBuilder body needsChunked) = do
    (header, hdrLen) <- composeHeaderBuilder ver s hs rspidxhdr needsChunked
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
    --              small adjustment to only count the body
    return (Just s, Just $ len - fromIntegral hdrLen)

----------------------------------------------------------------

sendRsp conn _ th ver s hs rspidxhdr _ _ (RspStream streamingBody needsChunked) = do
    (header, hdrLen) <- composeHeaderBuilder ver s hs rspidxhdr needsChunked
    (recv, finish) <-
        newByteStringBuilderRecv $
            reuseBufferStrategy $
                toBuilderBuffer $
                    connWriteBuffer conn
    -- We'll be counting how many bytes we send with this 'IORef'
    sizeCounter <- newIORef (0 :: Integer)
    let send builder = do
            popper <- recv builder
            let loop = do
                    bs <- popper
                    unless (S.null bs) $ do
                        sendFragment conn th bs
                        -- add amount of bytes to count
                        S.length bs `addToCounter` sizeCounter
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
    finalSize <- readIORef sizeCounter
    --              small adjustment to only count the body
    return (Just s, Just $ finalSize - fromIntegral hdrLen)
  where
    addToCounter :: Int -> IORef Integer -> IO ()
    addToCounter bytes ref =
        atomicModifyIORef' ref $ \old ->
            (old + fromIntegral bytes, ())

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
    efinfo <- E.try $ getFileInfo ii path
    case efinfo of
        Left (_ex :: E.IOException) ->
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
    hs = replaceHeader Header.hContentType "text/plain; charset=utf-8" hs0
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

-- | We infer from the request whether the connection should be persisted.
checkReqConnectionHeader :: Bool -> IndexedHeader -> Bool
checkReqConnectionHeader isHttp11 reqidxhdr =
    case reqidxhdr ! fromEnum ReqConnection of
        -- If no "Connection" header, then default: HTTP/1.1 == persist
        Nothing -> isHttp11
        Just val ->
            let connValue = CI.foldCase val
             in if isHttp11
                    then connValue /= "close"
                    else connValue == "keep-alive"

----------------------------------------------------------------

-- | Only checks for status codes, NOT for methods.
--
-- This is by design and some handling relies on HEAD being a separate check.
hasBody :: H.Status -> Bool
hasBody s =
    sc /= 204
        && sc /= 304
        && sc >= 200
  where
    sc = H.statusCode s

----------------------------------------------------------------

-- | We ASSUME there's no middleware that will chunk the transfer, so
-- we'll add it to the headers if there's no other encoding, or add it
-- to the end in case it is.
-- (e.g. if a 'Middleware' were to add "Transfer-Encoding: gzip")
addTransferEncoding :: IndexedHeader -> H.ResponseHeaders -> H.ResponseHeaders
addTransferEncoding rspidxhdr =
    case rspidxhdr ! fromEnum ResTransferEncoding of
        Nothing -> ((Header.hTransferEncoding, "chunked") :)
        Just value -> replaceHeader Header.hTransferEncoding (value <> ", chunked")

addDate
    :: IO D.GMTDate -> IndexedHeader -> H.ResponseHeaders -> IO H.ResponseHeaders
addDate getdate rspidxhdr hdrs = case rspidxhdr ! fromEnum ResDate of
    Nothing -> do
        gmtdate <- getdate
        return $ (Header.hDate, gmtdate) : hdrs
    Just _ -> return hdrs

----------------------------------------------------------------

{-# INLINE addServer #-}
addServer
    :: HeaderValue -> IndexedHeader -> H.ResponseHeaders -> H.ResponseHeaders
addServer serverName rspidxhdr hdrs =
    case (serverName, serverHdr) of
        -- empty string means there shouldn't be a "Server" header
        ("", Nothing) -> hdrs
        ("", _) -> filter ((/= Header.hServer) . fst) hdrs
        -- Anything else should set the "Server" header if it isn't already set
        (_, Nothing) -> (Header.hServer, serverName) : hdrs
        _ -> hdrs
  where
    serverHdr = rspidxhdr ! fromEnum ResServer

addAltSvc :: Settings -> H.ResponseHeaders -> H.ResponseHeaders
addAltSvc settings hs = case settingsAltSvc settings of
    Nothing -> hs
    Just v -> ("Alt-Svc", v) : hs

----------------------------------------------------------------

-- | Replaces a header, instead of just adding it which might lead to
-- duplicate entries of the same header name.
--
-- >>> replaceHeader "Content-Type" "new" [("content-type","old")]
-- [("Content-Type","new")]
replaceHeader
    :: H.HeaderName -> HeaderValue -> H.ResponseHeaders -> H.ResponseHeaders
replaceHeader k v hdrs = (k, v) : filter ((/= k) . fst) hdrs

----------------------------------------------------------------

composeHeaderBuilder
    :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> IndexedHeader -> Bool -> IO (Builder, Int)
composeHeaderBuilder ver s hs rspidxhdr shouldChunk = do
    bs <- composeHeader ver s finalHdrs
    pure (byteString bs, S.length bs)
  where
    finalHdrs
        | shouldChunk = addTransferEncoding rspidxhdr hs
        | otherwise = hs
