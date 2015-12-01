{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Response (
    sendResponse
  , sanitizeHeaderValue -- for testing
  , fileRange -- for testing
  , warpVersion
  , addDate
  , addServer
  , hasBody
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

#ifndef MIN_VERSION_http_types
#define MIN_VERSION_http_types(x,y,z) 1
#endif

import Blaze.ByteString.Builder.HTTP (chunkedTransferEncoding, chunkedTransferTerminator)
#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.Exception
import Control.Monad (unless, when)
import Data.Array ((!))
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.ByteString.Builder (byteString, Builder)
import Data.ByteString.Builder.Extra (flush)
import qualified Data.CaseInsensitive as CI
import Data.Function (on)
import Data.List (deleteBy)
import Data.Maybe
#if MIN_VERSION_base(4,5,0)
# if __GLASGOW_HASKELL__ < 709
import Data.Monoid (mempty)
# endif
import Data.Monoid ((<>))
#else
import Data.Monoid (mappend, mempty)
#endif
import Data.Streaming.Blaze (newBlazeRecv, reuseBufferStrategy)
import Data.Version (showVersion)
import Data.Word8 (_cr, _lf)
import qualified Network.HTTP.Types as H
#if MIN_VERSION_http_types(0,9,0)
import qualified Network.HTTP.Types.Header as H
#endif
import Network.Wai
import Network.Wai.Handler.Warp.Buffer (toBuilderBuffer)
import qualified Network.Wai.Handler.Warp.Date as D
import qualified Network.Wai.Handler.Warp.FdCache as F
import qualified Network.Wai.Handler.Warp.FileInfoCache as I
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.IO (toBufIOWith)
import Network.Wai.Handler.Warp.ResponseHeader
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai.Handler.Warp.Types
import Network.Wai.Internal
import qualified Paths_warp

#if !MIN_VERSION_base(4,5,0)
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

-- $setup
-- >>> :set -XOverloadedStrings

----------------------------------------------------------------

fileRange :: H.Status -> H.ResponseHeaders -> FilePath
          -> Maybe FilePart -> Maybe HeaderValue
          -> (FilePath -> IO I.FileInfo)
          -> IO (Either IOException
                        (H.Status, H.ResponseHeaders, Integer, Integer))
fileRange s0 hs0 path Nothing mRange get = do
    efinfo <- try (get path)
    case efinfo of
        Left  e     -> return $ Left e
        Right finfo -> do
            let ret = fileRangeSized s0 hs0 Nothing mRange $ I.fileInfoSize finfo
            return $ Right ret
fileRange s0 hs0 _ mPart@(Just part) mRange _ =
    return . Right $ fileRangeSized s0 hs0 mPart mRange size
  where
    size = filePartFileSize part

fileRangeSized :: H.Status -> H.ResponseHeaders
               -> Maybe FilePart -> Maybe HeaderValue -> Integer
               -> (H.Status, H.ResponseHeaders, Integer, Integer)
fileRangeSized s0 hs0 mPart mRange fileSize = (s, hs, beg, len)
  where
    part = fromMaybe (chooseFilePart fileSize mRange) mPart
    beg = filePartOffset part
    len = filePartByteCount part
    (s, hs) = adjustForFilePart s0 hs0 $ FilePart beg len fileSize

----------------------------------------------------------------

-- | Sending a HTTP response to 'Connection' according to 'Response'.
--
--   Applications/middlewares MUST specify a proper 'H.ResponseHeaders'.
--   so that inconsistency does not happen.
--   No header is deleted by this function.
--
--   Especially, Applications/middlewares MUST take care of
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
--   ['responseFile' :: 'H.Status' -> 'H.ResponseHeaders' -> 'FilePath' -> 'Maybe' 'FilePart' -> 'Response']
--     HTTP response body is sent by sendfile() for GET method.
--     HTTP response body is not sent by HEAD method.
--     Applications are categorized into simple and sophisticated.
--     Simple applications should specify 'Nothing' to
--     'Maybe' 'FilePart'. The size of the specified file is obtained
--     by disk access. Then Range is handled.
--     Sophisticated applications should specify 'Just' to
--     'Maybe' 'FilePart'. They should treat Range (and If-Range) by
--     themselves. In both cases,
--     Content-Length and Content-Range (if necessary) are automatically
--     added into the HTTP response header.
--     If Content-Length and Content-Range exist in the HTTP response header,
--     they would cause inconsistency.
--     Status is also changed to 206 (Partial Content) if necessary.
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

sendResponse :: ByteString -- ^ default server value
             -> Connection
             -> InternalInfo
             -> Request -- ^ HTTP request.
             -> IndexedHeader -- ^ Indexed header of HTTP request.
             -> IO ByteString -- ^ source from client, for raw response
             -> Response -- ^ HTTP response including status code and response header.
             -> IO Bool -- ^ Returing True if the connection is persistent.
sendResponse defServer conn ii req reqidxhdr src response = do
    hs <- addServerAndDate hs0
    if hasBody req s then do
        -- See definition of rsp below for proper body stripping.
        sendRsp conn ii ver s hs rsp
        T.tickle th
        return ret
      else do
        sendResponseNoBody conn ver s hs
        T.tickle th
        return isPersist
  where
    ver = httpVersion req
    s = responseStatus response
    hs0 = sanitizeHeaders $ responseHeaders response
    rspidxhdr = indexResponseHeader hs0
    th = threadHandle ii
    dc = dateCacher ii
    addServerAndDate = addDate dc rspidxhdr . addServer defServer rspidxhdr
    mRange = reqidxhdr ! idxRange
    (isPersist,isChunked) = infoFromRequest req reqidxhdr
    (isKeepAlive, needsChunked) = infoFromResponse rspidxhdr (isPersist,isChunked)
    rsp = case response of
        ResponseFile _ _ path mPart -> RspFile path mPart mRange (T.tickle th)
        ResponseBuilder _ _ b       -> RspBuilder b needsChunked
        ResponseStream _ _ fb       -> RspStream fb needsChunked th
        ResponseRaw raw _           -> RspRaw raw src (T.tickle th)
    ret = case response of
        ResponseFile    {} -> isPersist
        ResponseBuilder {} -> isKeepAlive
        ResponseStream  {} -> isKeepAlive
        ResponseRaw     {} -> False

----------------------------------------------------------------

sanitizeHeaders :: H.ResponseHeaders -> H.ResponseHeaders
sanitizeHeaders = map (sanitize <$>)
  where
    sanitize v
      | containsNewlines v = sanitizeHeaderValue v -- slow path
      | otherwise          = v                     -- fast path

{-# INLINE containsNewlines #-}
containsNewlines :: ByteString -> Bool
containsNewlines = S.any (\w -> w == _cr || w == _lf)

{-# INLINE sanitizeHeaderValue #-}
sanitizeHeaderValue :: ByteString -> ByteString
sanitizeHeaderValue v = case S8.lines $ S.filter (/= _cr) v of
    []     -> ""
    x : xs -> S8.intercalate "\r\n" (x : mapMaybe addSpaceIfMissing xs)
  where
    addSpaceIfMissing line = case S8.uncons line of
        Nothing                           -> Nothing
        Just (first, _)
          | first == ' ' || first == '\t' -> Just line
          | otherwise                     -> Just $ " " <> line

----------------------------------------------------------------

data Rsp = RspFile FilePath (Maybe FilePart) (Maybe HeaderValue) (IO ())
         | RspBuilder Builder Bool
         | RspStream StreamingBody Bool T.Handle
         | RspRaw (IO ByteString -> (ByteString -> IO ()) -> IO ()) (IO ByteString) (IO ())

----------------------------------------------------------------

sendRsp :: Connection
        -> InternalInfo
        -> H.HttpVersion
        -> H.Status
        -> H.ResponseHeaders
        -> Rsp
        -> IO ()
sendRsp conn ii ver s0 hs0 (RspFile path mPart mRange hook) = do
    ex <- fileRange s0 hs0 path mPart mRange get
    case ex of
        Left _ex ->
#ifdef WARP_DEBUG
          print _ex >>
#endif
          sendRsp conn ii ver s2 hs2 (RspBuilder body True)
        Right (s, hs, beg, len)
          | len >= 0 -> do
            lheader <- composeHeader ver s hs
#ifdef WINDOWS
            let fid = FileId path Nothing
                hook' = hook
#else
            (mfd, hook') <- case mfdc of
               -- settingsFdCacheDuration is 0
               Nothing  -> return (Nothing, hook)
               Just fdc -> do
                  (fd, fresher) <- F.getFd fdc path
                  return (Just fd, hook >> fresher)
            let fid = FileId path mfd
#endif
            connSendFile conn fid beg len hook' [lheader]
          | otherwise ->
            sendRsp conn ii ver H.status416
                (filter (\(k, _) -> k /= "content-length") hs)
                (RspBuilder mempty True)
  where
    s2 = H.status404
    hs2 =  replaceHeader H.hContentType "text/plain; charset=utf-8" hs0
    body = byteString "File not found"
    mfdc = fdCacher ii
    get = fileInfo ii

----------------------------------------------------------------

sendRsp conn _ ver s hs (RspBuilder body needsChunked) = do
    header <- composeHeaderBuilder ver s hs needsChunked
    let hdrBdy
         | needsChunked = header <> chunkedTransferEncoding body
                                 <> chunkedTransferTerminator
         | otherwise    = header <> body
        buffer = connWriteBuffer conn
        size = connBufferSize conn
    toBufIOWith buffer size (connSendAll conn) hdrBdy

----------------------------------------------------------------

sendRsp conn _ ver s hs (RspStream streamingBody needsChunked th) = do
    header <- composeHeaderBuilder ver s hs needsChunked
    (recv, finish) <- newBlazeRecv $ reuseBufferStrategy
                    $ toBuilderBuffer (connWriteBuffer conn) (connBufferSize conn)
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

----------------------------------------------------------------

sendRsp conn _ _ _ _ (RspRaw withApp src tickle) =
    withApp recv send
  where
    recv = do
        bs <- src
        unless (S.null bs) tickle
        return bs
    send bs = connSendAll conn bs >> tickle

----------------------------------------------------------------

sendResponseNoBody :: Connection
                   -> H.HttpVersion
                   -> H.Status
                   -> H.ResponseHeaders
                   -> IO ()
sendResponseNoBody conn ver s hs = composeHeader ver s hs >>= connSendAll conn

----------------------------------------------------------------
----------------------------------------------------------------

-- | Use 'connSendAll' to send this data while respecting timeout rules.
sendFragment :: Connection -> T.Handle -> ByteString -> IO ()
sendFragment Connection { connSendAll = send } th bs = do
    T.resume th
    send bs
    T.pause th
    -- We pause timeouts before passing control back to user code. This ensures
    -- that a timeout will only ever be executed when Warp is in control. We
    -- also make sure to resume the timeout after the completion of user code
    -- so that we can kill idle connections.

----------------------------------------------------------------

infoFromRequest :: Request -> IndexedHeader -> (Bool  -- isPersist
                                               ,Bool) -- isChunked
infoFromRequest req reqidxhdr = (checkPersist req reqidxhdr, checkChunk req)

checkPersist :: Request -> IndexedHeader -> Bool
checkPersist req reqidxhdr
    | ver == H.http11 = checkPersist11 conn
    | otherwise       = checkPersist10 conn
  where
    ver = httpVersion req
    conn = reqidxhdr ! idxConnection
    checkPersist11 (Just x)
        | CI.foldCase x == "close"      = False
    checkPersist11 _                    = True
    checkPersist10 (Just x)
        | CI.foldCase x == "keep-alive" = True
    checkPersist10 _                    = False

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
infoFromResponse :: IndexedHeader -> (Bool,Bool) -> (Bool,Bool)
infoFromResponse rspidxhdr (isPersist,isChunked) = (isKeepAlive, needsChunked)
  where
    needsChunked = isChunked && not hasLength
    isKeepAlive = isPersist && (isChunked || hasLength)
    hasLength = isJust $ rspidxhdr ! idxContentLength

----------------------------------------------------------------

hasBody :: Request -> H.Status -> Bool
hasBody req s = not isHead
             && sc /= 204
             && sc /= 304
             && sc >= 200
  where
    sc = H.statusCode s
    isHead = requestMethod req == H.methodHead

----------------------------------------------------------------

addTransferEncoding :: H.ResponseHeaders -> H.ResponseHeaders
#if MIN_VERSION_http_types(0,9,0)
addTransferEncoding hdrs = (H.hTransferEncoding, "chunked") : hdrs
#else
addTransferEncoding hdrs = ("transfer-encoding", "chunked") : hdrs
#endif

addDate :: D.DateCache -> IndexedHeader -> H.ResponseHeaders -> IO H.ResponseHeaders
addDate dc rspidxhdr hdrs = case rspidxhdr ! idxDate of
    Nothing -> do
        gmtdate <- D.getDate dc
        return $ (H.hDate, gmtdate) : hdrs
    Just _ -> return hdrs

----------------------------------------------------------------

-- | The version of Warp.
warpVersion :: String
warpVersion = showVersion Paths_warp.version

addServer :: HeaderValue -> IndexedHeader -> H.ResponseHeaders -> H.ResponseHeaders
addServer serverName rspidxhdr hdrs = case rspidxhdr ! idxServer of
    Nothing -> (H.hServer, serverName) : hdrs
    _       -> hdrs

----------------------------------------------------------------

-- |
--
-- >>> replaceHeader "Content-Type" "new" [("content-type","old")]
-- [("Content-Type","new")]
replaceHeader :: H.HeaderName -> HeaderValue -> H.ResponseHeaders -> H.ResponseHeaders
replaceHeader k v hdrs = (k,v) : deleteBy ((==) `on` fst) (k,v) hdrs

----------------------------------------------------------------

composeHeaderBuilder :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> IO Builder
composeHeaderBuilder ver s hs True =
    byteString <$> composeHeader ver s (addTransferEncoding hs)
composeHeaderBuilder ver s hs False =
    byteString <$> composeHeader ver s hs
