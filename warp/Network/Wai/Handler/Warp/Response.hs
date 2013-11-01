{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Network.Wai.Handler.Warp.Response (
    sendResponse
  , fileRange -- for testing
  ) where

import Blaze.ByteString.Builder (fromByteString, Builder, toByteStringIO, flush)
import Blaze.ByteString.Builder.HTTP (chunkedTransferEncoding, chunkedTransferTerminator)
import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class (liftIO)
import Data.Array ((!))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.CaseInsensitive as CI
import Data.Conduit
import Data.Conduit.Blaze (builderToByteString)
import qualified Data.Conduit.List as CL
import Data.Maybe (isJust, listToMaybe)
import Data.Monoid (mappend)
import Network.HTTP.Attoparsec (parseByteRanges)
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp.Header
import Network.Wai.Handler.Warp.ResponseHeader
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai.Handler.Warp.Types
import Network.Wai.Internal
import Numeric (showInt)
import qualified System.PosixCompat.Files as P

----------------------------------------------------------------

fileRange :: H.Status -> H.ResponseHeaders -> FilePath
           -> Maybe FilePart -> Maybe HeaderValue
          -> IO (Either SomeException
                        (H.Status, H.ResponseHeaders, Integer, Integer))
fileRange s0 hs0 path mPart mRange = liftIO . try $ do
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

----------------------------------------------------------------

sendResponse conn ii restore req reqidxhdr (ResponseFile s0 hs0 path mPart) =
    restore $ fileRange s0 hs path mPart mRange >>= sendResponseEither
  where
    hs = addServer rspidxhdr $ addAcceptRanges hs0
    rspidxhdr = indexResponseHeader hs0
    th = threadHandle ii
    mRange = reqidxhdr ! idxRange

    sendResponseEither (Right (s, lengthyHeaders, beg, len))
      | hasBody s req = liftIO $ do
          lheader <- composeHeader version s lengthyHeaders
          connSendFile conn path beg len (T.tickle th) [lheader]
          T.tickle th
          return isPersist
      | otherwise = liftIO $ sendResponseNoBody conn th version s hs isPersist
      where
        version = httpVersion req
        (isPersist,_) = infoFromRequest req reqidxhdr

    sendResponseEither (Left (SomeException _)) =
        sendResponse conn ii restore req reqidxhdr notFound
      where
        notFound = responseLBS H.status404 [(H.hContentType, "text/plain")] "File not found"

----------------------------------------------------------------

sendResponse conn ii restore req reqidxhdr (ResponseBuilder s hs0 b)
  | hasBody s req = restore $ do
      header <- composeHeaderBuilder version s hs needsChunked
      let body
            | needsChunked = header `mappend` chunkedTransferEncoding b
                                    `mappend` chunkedTransferTerminator
            | otherwise    = header `mappend` b
      flip toByteStringIO body $ \bs -> do
          connSendAll conn bs
          T.tickle th
      return isKeepAlive
  | otherwise = restore $ sendResponseNoBody conn th version s hs isPersist
  where
    hs = addServer rspidxhdr hs0
    rspidxhdr = indexResponseHeader hs0
    th = threadHandle ii
    version = httpVersion req
    reqinfo@(isPersist,_) = infoFromRequest req reqidxhdr
    (isKeepAlive, needsChunked) = infoFromResponse rspidxhdr reqinfo

----------------------------------------------------------------

sendResponse conn ii restore req reqidxhdr (ResponseSource s hs0 withBodyFlush)
  | hasBody s req = withBodyFlush $ \bodyFlush -> restore $ do
      header <- liftIO $ composeHeaderBuilder version s hs needsChunked
      let src = CL.sourceList [header] `mappend` cbody bodyFlush
      src $$ builderToByteString =$ connSink conn th
      return isKeepAlive
  | otherwise = withBodyFlush $ \_bodyFlush ->
      -- make sure any cleanup is called
      restore $ sendResponseNoBody conn th version s hs isPersist
  where
    hs = addServer rspidxhdr hs0
    rspidxhdr = indexResponseHeader hs0
    th = threadHandle ii
    version = httpVersion req
    reqinfo@(isPersist,_) = infoFromRequest req reqidxhdr
    (isKeepAlive, needsChunked) = infoFromResponse rspidxhdr reqinfo
    cbody bodyFlush = if needsChunked then body $= chunk else body
      where
        body = mapOutput (\x -> case x of
                        Flush -> flush
                        Chunk builder -> builder)
               bodyFlush
    -- FIXME perhaps alloca a buffer per thread and reuse that in all
    -- functions below. Should lessen greatly the GC burden (I hope)
    chunk :: Conduit Builder IO Builder
    chunk = await >>= maybe (yield chunkedTransferTerminator) (\x -> yield (chunkedTransferEncoding x) >> chunk)

----------------------------------------------------------------

sendResponseNoBody :: Connection -> T.Handle
                   -> H.HttpVersion -> H.Status -> H.ResponseHeaders
                   -> Bool -> IO Bool
sendResponseNoBody conn th version s hs isPersist = do
    composeHeader version s hs >>= connSendAll conn
    T.tickle th
    return isPersist

----------------------------------------------------------------
----------------------------------------------------------------

-- | Use 'connSendAll' to send this data while respecting timeout rules.
connSink :: Connection -> T.Handle -> Sink ByteString IO ()
connSink Connection { connSendAll = send } th =
    sink
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

----------------------------------------------------------------

addServer :: IndexedHeader -> H.ResponseHeaders -> H.ResponseHeaders
addServer rspidxhdr hdrs = case rspidxhdr ! idxServer of
    Nothing -> (hServer, defaultServerValue) : hdrs
    _       -> hdrs

----------------------------------------------------------------

composeHeaderBuilder :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> IO Builder
composeHeaderBuilder ver s hs True =
    fromByteString <$> composeHeader ver s (addTransferEncoding hs)
composeHeaderBuilder ver s hs False =
    fromByteString <$> composeHeader ver s hs
