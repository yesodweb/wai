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

import Blaze.ByteString.Builder (fromByteString, Builder, flush)
import Blaze.ByteString.Builder.HTTP (chunkedTransferEncoding, chunkedTransferTerminator)
import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Array ((!))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.CaseInsensitive as CI
import Data.Conduit
import Data.Conduit.Blaze (unsafeBuilderToByteString)
import Data.Function (on)
import Data.List (deleteBy)
import Data.Maybe (isJust, listToMaybe)
#if MIN_VERSION_base(4,5,0)
import Data.Monoid ((<>))
#else
import Data.Monoid (mappend)
#endif
import Data.Version (showVersion)
import Network.HTTP.Attoparsec (parseByteRanges)
import qualified Network.HTTP.Types as H
import Network.Wai
import qualified Network.Wai.Handler.Warp.Date as D
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.Buffer (toBlazeBuffer)
import Network.Wai.Handler.Warp.IO (toBufIOWith)
import Network.Wai.Handler.Warp.ResponseHeader
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

----------------------------------------------------------------

fileRange :: H.Status -> H.ResponseHeaders -> FilePath
           -> Maybe FilePart -> Maybe HeaderValue
          -> IO (Either SomeException
                        (H.Status, H.ResponseHeaders, Integer, Integer))
fileRange s0 hs0 path mPart mRange = try $ do
    fileSize <- checkFileSize mPart
    let (beg, end, len, isEntire) = checkPartRange fileSize mPart mRange
    let hs1 = addContentLength len hs0
        hs | isEntire  = hs1
           | otherwise = addContentRange beg end fileSize hs1
        s  | isEntire  = s0
           | otherwise = H.status206
    return (s, hs, beg, len)
 where
    checkFileSize Nothing = fromIntegral . P.fileSize <$> P.getFileStatus path
    checkFileSize (Just part) = return $ filePartFileSize part

checkPartRange :: Integer -> Maybe FilePart -> Maybe HeaderValue
               -> (Integer, Integer, Integer, Bool)
checkPartRange fileSize mPart mRange = checkPart mPart mRange
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
    checkRange (H.ByteRangeFromTo beg end) = fromRange beg end
    checkRange (H.ByteRangeSuffix count)   = fromRange (fileSize - count) (fileSize - 1)

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
--   The Server header is added if not exist in HTTP response header.
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
--     HTTP response body is created from 'Source'.
--     Typically, Transfer-Encoding: chunked is used.
--     If Content-Length is specified, Transfer-Encoding: chunked is not used.
--
--   ['responseSource' :: 'H.Status' -> 'H.ResponseHeaders' -> 'Source' 'IO' ('Flush' 'Builder') -> 'Response']
--     HTTP response body is created from 'Builder'.
--     Typically, Transfer-Encoding: chunked is used.
--     If Content-Length is specified, Transfer-Encoding: chunked is not used.

sendResponse :: Connection
             -> InternalInfo
             -> (forall a. IO a -> IO a) -- ^ Restore masking state.
             -> Request -- ^ HTTP request.
             -> IndexedHeader -- ^ Indexed header of HTTP request.
             -> Response -- ^ HTTP response including status code and response header.
             -> IO Bool -- ^ Returing True if the connection is persistent.
sendResponse conn ii restore req reqidxhdr response = restore $ do
    hs <- addServerAndDate hs0
    if hasBody s req then do
        sendRsp conn ver s hs rsp
        T.tickle th
        return ret
      else do
        sendResponseNoBody conn ver s hs response
        T.tickle th
        return isPersist
  where
    ver = httpVersion req
    s = responseStatus response
    hs0 = responseHeaders response
    rspidxhdr = indexResponseHeader hs0
    th = threadHandle ii
    dc = dateCacher ii
    addServerAndDate = addDate dc . addServer rspidxhdr
    mRange = reqidxhdr ! idxRange
    reqinfo@(isPersist,_) = infoFromRequest req reqidxhdr
    (isKeepAlive, needsChunked) = infoFromResponse rspidxhdr reqinfo
    rsp = case response of
        ResponseFile _ _ path mPart -> RspFile path mPart mRange (T.tickle th)
        ResponseBuilder _ _ b       -> RspBuilder b needsChunked
        ResponseSource _ _ fb       -> RspSource fb needsChunked th
    ret = case response of
        ResponseFile _ _ _ _  -> isPersist
        ResponseBuilder _ _ _ -> isKeepAlive
        ResponseSource _ _ _  -> isKeepAlive

----------------------------------------------------------------

data Rsp = RspFile FilePath (Maybe FilePart) (Maybe HeaderValue) (IO ())
         | RspBuilder Builder Bool
         | RspSource (forall b. WithSource IO (Flush Builder) b) Bool T.Handle

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
        Left _ -> sendRsp conn ver s2 hs2 (RspBuilder body True)
        Right (s, hs1, beg, len) -> do
            lheader <- composeHeader ver s hs1
            connSendFile conn path beg len hook [lheader]
  where
    hs = addAcceptRanges hs0
    s2 = H.status404
    hs2 =  replaceHeader H.hContentType "text/plain" hs0
    body = fromByteString "File not found"

----------------------------------------------------------------

sendRsp conn ver s hs (RspBuilder body needsChunked) = do
    header <- composeHeaderBuilder ver s hs needsChunked
    let hdrBdy
         | needsChunked = header <> chunkedTransferEncoding body
                                 <> chunkedTransferTerminator
         | otherwise    = header <> body
        buffer = connBuffer conn
        size = connBufferSize conn
    toBufIOWith buffer size (connSendAll conn) hdrBdy

----------------------------------------------------------------

sendRsp conn ver s hs (RspSource withBodyFlush needsChunked th) = withBodyFlush $ \bodyFlush -> do
    header <- composeHeaderBuilder ver s hs needsChunked
    let src = yield header >> cbody bodyFlush
    buffer <- toBlazeBuffer (connBuffer conn) (connBufferSize conn)
    src $$ unsafeBuilderToByteString (return buffer) =$ connSink conn th
  where
    cbody bodyFlush = if needsChunked then body $= chunk else body
      where
        body = mapOutput (\x -> case x of
                        Flush -> flush
                        Chunk builder -> builder)
               bodyFlush
    chunk :: Conduit Builder IO Builder
    chunk = await >>= maybe (yield chunkedTransferTerminator) (\x -> yield (chunkedTransferEncoding x) >> chunk)

----------------------------------------------------------------

sendResponseNoBody :: Connection
                   -> H.HttpVersion
                   -> H.Status
                   -> H.ResponseHeaders
                   -> Response
                   -> IO ()
sendResponseNoBody conn ver s hs (ResponseSource _ _ withBodyFlush) =
    withBodyFlush $ \_bodyFlush ->
       composeHeader ver s hs >>= connSendAll conn
sendResponseNoBody conn ver s hs _ =
    composeHeader ver s hs >>= connSendAll conn

----------------------------------------------------------------
----------------------------------------------------------------

-- | Use 'connSendAll' to send this data while respecting timeout rules.
connSink :: Connection -> T.Handle -> Sink ByteString IO ()
connSink Connection { connSendAll = send } th = sink
  where
    sink = await >>= maybe close push
    close = liftIO (T.resume th)
    push x = do
        liftIO $ T.resume th
        liftIO $ send x
        liftIO $ T.pause th
        sink
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
      : showInt beg
      ( '-'
      : showInt end
      ( '/'
      : showInt total ""))

addDate :: D.DateCache -> H.ResponseHeaders -> IO H.ResponseHeaders
addDate dc hdrs = do
    gmtdate <- D.getDate dc
    return $ (H.hDate, gmtdate) : hdrs

----------------------------------------------------------------

-- | The version of Warp.
warpVersion :: String
warpVersion = showVersion Paths_warp.version

defaultServerValue :: HeaderValue
defaultServerValue = B.pack $ "Warp/" ++ warpVersion

addServer :: IndexedHeader -> H.ResponseHeaders -> H.ResponseHeaders
addServer rspidxhdr hdrs = case rspidxhdr ! idxServer of
    Nothing -> (hServer, defaultServerValue) : hdrs
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
