{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Response (
    sendResponse
  , fileRange -- for testing
  , warpVersion
  , defaultServerValue
  ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

import Blaze.ByteString.Builder (fromByteString, Builder, flush)
import Blaze.ByteString.Builder.HTTP (chunkedTransferEncoding, chunkedTransferTerminator)
import Control.Applicative
import Control.Exception
import Data.Array ((!))
import Data.ByteString (ByteString)
import Data.Streaming.Blaze (newBlazeRecv, reuseBufferStrategy)
import qualified Data.ByteString as S
import Control.Monad (unless, when)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.CaseInsensitive as CI
import Data.Function (on)
import Data.List (deleteBy)
import Data.Maybe (isJust, listToMaybe)
#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>), mempty)
#else
import Data.Monoid (mappend, mempty)
#endif
import Data.Version (showVersion)
import qualified Network.HTTP.Types as H
import Network.Wai
import qualified Network.Wai.Handler.Warp.Date as D
import Network.Wai.Handler.Warp.Buffer (toBlazeBuffer)
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.IO (toBufIOWith)
import Network.Wai.Handler.Warp.ResponseHeader
import Network.Wai.Handler.Warp.RequestHeader (parseByteRanges)
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai.Handler.Warp.Types
import Network.Wai.Internal
import Numeric (showInt)
import qualified Paths_warp
import qualified System.PosixCompat.Files as P

#if !MIN_VERSION_base(4,5,0)
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

-- $setup
-- >>> :set -XOverloadedStrings

mapRight :: (b -> c) -> Either a b -> Either a c
mapRight f eith = case eith of
    Right x -> Right (f x)
    Left l -> Left l

----------------------------------------------------------------

fileRange :: H.Status -> H.ResponseHeaders -> FilePath
          -> Maybe FilePart -> Maybe HeaderValue
          -> IO (Either IOException
                        (H.Status, H.ResponseHeaders, Integer, Integer))
fileRange s0 hs0 path Nothing mRange =
    mapRight (fileRangeSized s0 hs0 Nothing mRange . fromIntegral . P.fileSize) <$>
    try (P.getFileStatus path)
fileRange s0 hs0 _ mPart@(Just part) mRange =
    return . Right $ fileRangeSized s0 hs0 mPart mRange size
  where
    size = filePartFileSize part

fileRangeSized :: H.Status -> H.ResponseHeaders
               -> Maybe FilePart -> Maybe HeaderValue -> Integer
               -> (H.Status, H.ResponseHeaders, Integer, Integer)
fileRangeSized s0 hs0 mPart mRange fileSize = (s, hs, beg, len)
  where
    (beg, end, len, isEntire) = checkPartRange fileSize mPart mRange
    hs1 = addContentLength len hs0
    hs | isEntire  = hs1
       | otherwise = addContentRange beg end fileSize hs1
    s  | isEntire  = s0
       | otherwise = H.status206

checkPartRange :: Integer -> Maybe FilePart -> Maybe HeaderValue
               -> (Integer, Integer, Integer, Bool)
checkPartRange fileSize = checkPart
  where
    checkPart Nothing Nothing = (0, fileSize - 1, fileSize, True)
    checkPart Nothing (Just range) = case parseByteRanges range >>= listToMaybe of
        -- Range is broken
        Nothing              -> (0, fileSize - 1, fileSize, True)
        Just hrange          -> checkRange hrange
    -- Ignore Range if FilePart is specified.
    -- We assume that an application handled Range and specified
    -- FilePart.
    checkPart (Just part) _   = (beg, end, len, isEntire)
      where
        beg = filePartOffset part
        len = filePartByteCount part
        end = beg + len - 1
        isEntire = beg == 0 && len == fileSize

    checkRange (H.ByteRangeFrom   beg)     = fromRange beg (fileSize - 1)
    checkRange (H.ByteRangeFromTo beg end) = fromRange beg (min (fileSize - 1) end)
    checkRange (H.ByteRangeSuffix count)   = fromRange (max 0 (fileSize - count)) (fileSize - 1)

    fromRange beg end = (beg, end, len, isEntire)
      where
        len = end - beg + 1
        isEntire = beg == 0 && len == fileSize

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
--     HTTP response body is sent by sendfile().
--     Applications are categorized into simple and sophisticated.
--     Simple applications should specify 'Nothing' to
--     'Maybe' 'FilePart'. The size of the specified file is obtained
--     by disk access. Then Range is handled.
--     Sophisticated applications should specify 'Just' to
--     'Maybe' 'FilePart'. They should treat Range (and If-Range) by
--     thierselves. In both cases,
--     Content-Length and Content-Range (if necessary) are automatically
--     added into the HTTP response header.
--     If Content-Length and Content-Range exist in the HTTP response header,
--     they would cause inconsistency.
--     Status is also changed to 206 if necessary.
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
    if hasBody s req then do
        sendRsp conn ver s hs rsp
        T.tickle th
        return ret
      else do
        sendResponseNoBody conn ver s hs
        T.tickle th
        return isPersist
  where
    ver = httpVersion req
    s = responseStatus response
    hs0 = responseHeaders response
    rspidxhdr = indexResponseHeader hs0
    th = threadHandle ii
    dc = dateCacher ii
    addServerAndDate = addDate dc rspidxhdr . addServer defServer rspidxhdr
    mRange = reqidxhdr ! idxRange
    reqinfo@(isPersist,_) = infoFromRequest req reqidxhdr
    (isKeepAlive, needsChunked) = infoFromResponse rspidxhdr reqinfo
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

data Rsp = RspFile FilePath (Maybe FilePart) (Maybe HeaderValue) (IO ())
         | RspBuilder Builder Bool
         | RspStream StreamingBody Bool T.Handle
         | RspRaw (IO ByteString -> (ByteString -> IO ()) -> IO ()) (IO ByteString) (IO ())

----------------------------------------------------------------

sendRsp :: Connection
        -> H.HttpVersion
        -> H.Status
        -> H.ResponseHeaders
        -> Rsp
        -> IO ()
sendRsp conn ver s0 hs0 (RspFile path mPart mRange hook) = do
    ex <- fileRange s0 hs path mPart mRange
    case ex of
        Left _ex ->
#ifdef WARP_DEBUG
          print _ex >>
#endif
          sendRsp conn ver s2 hs2 (RspBuilder body True)
        Right (s, hs1, beg, len) | len > 0 -> do
            lheader <- composeHeader ver s hs1
            connSendFile conn path beg len hook [lheader]
          | otherwise -> do
            sendRsp conn ver H.status416 hs1 (RspBuilder mempty True)
  where
    hs = addAcceptRanges hs0
    s2 = H.status404
    hs2 =  replaceHeader H.hContentType "text/plain; charset=utf-8" hs0
    body = fromByteString "File not found"

----------------------------------------------------------------

sendRsp conn ver s hs (RspBuilder body needsChunked) = do
    header <- composeHeaderBuilder ver s hs needsChunked
    let hdrBdy
         | needsChunked = header <> chunkedTransferEncoding body
                                 <> chunkedTransferTerminator
         | otherwise    = header <> body
        buffer = connWriteBuffer conn
        size = connBufferSize conn
    toBufIOWith buffer size (connSendAll conn) hdrBdy

----------------------------------------------------------------

sendRsp conn ver s hs (RspStream streamingBody needsChunked th) = do
    header <- composeHeaderBuilder ver s hs needsChunked
    (recv, finish) <- newBlazeRecv $ reuseBufferStrategy
                    $ toBlazeBuffer (connWriteBuffer conn) (connBufferSize conn)
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

sendRsp conn _ _ _ (RspRaw withApp src tickle) =
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

hasBody :: H.Status -> Request -> Bool
hasBody s req = sc /= 204
             && sc /= 304
             && sc >= 200
             && method /= H.methodHead
  where
    sc = H.statusCode s
    method = requestMethod req

----------------------------------------------------------------

addAcceptRanges :: H.ResponseHeaders -> H.ResponseHeaders
addAcceptRanges hdrs = (hAcceptRanges, "bytes") : hdrs

addTransferEncoding :: H.ResponseHeaders -> H.ResponseHeaders
addTransferEncoding hdrs = (hTransferEncoding, "chunked") : hdrs

addContentLength :: Integer -> H.ResponseHeaders -> H.ResponseHeaders
addContentLength cl hdrs = (H.hContentLength, len) : hdrs
  where
    len = B.pack $ show cl

addContentRange :: Integer -> Integer -> Integer
                -> H.ResponseHeaders -> H.ResponseHeaders
addContentRange beg end total hdrs = (hContentRange, range) : hdrs
  where
    range = B.pack
      -- building with ShowS
      $ 'b' : 'y': 't' : 'e' : 's' : ' '
      : (if beg > end then ('*':) else
          (showInt beg)
          . ('-' :)
          . (showInt end))
      ( '/'
      : showInt total "")

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

defaultServerValue :: HeaderValue
defaultServerValue = B.pack $ "Warp/" ++ warpVersion

addServer :: HeaderValue -> IndexedHeader -> H.ResponseHeaders -> H.ResponseHeaders
addServer defaultServerValue' rspidxhdr hdrs = case rspidxhdr ! idxServer of
    Nothing -> (hServer, defaultServerValue') : hdrs
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
    fromByteString <$> composeHeader ver s (addTransferEncoding hs)
composeHeaderBuilder ver s hs False =
    fromByteString <$> composeHeader ver s hs
