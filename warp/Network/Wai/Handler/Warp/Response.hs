{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Network.Wai.Handler.Warp.Response (
    sendResponse
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
import qualified Network.Wai.Handler.Warp.ResponseHeader as RH
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai.Handler.Warp.Types
import Network.Wai.Internal
import Numeric (showInt)
import qualified System.PosixCompat.Files as P

----------------------------------------------------------------

-- Application should just specify FilePart.
-- Content-Length and Content-Range are automatically added.
-- Application should not be specify Content-Length and Content-Range
-- to avoid inconsistency.
fileRange :: IndexedHeader -> IndexedHeader
          -> H.Status -> H.ResponseHeaders
          -> FilePath -> Maybe FilePart
          -> IO (Either SomeException
                        (H.Status, H.ResponseHeaders, Integer, Integer))
fileRange reqidxhdr rspidxhdr s0 hs0 path mpart = liftIO . try $
    tryContentLength s0 hs0
 where
    mRange         = reqidxhdr ! idxRange
    mContentLength = rspidxhdr ! idxContentLength
    mContentRange  = rspidxhdr ! idxContentRange

    tryContentLength s' hdr' = do
        quad@(s, hdr, beg, len) <- tryMpart s' hdr' mpart
        -- We respect Content-Length if exists.
        -- The validity of Content-Length's value is Applications's
        -- responsibility.
        case mContentLength of
            Nothing -> return (s, addContentLength len hdr, beg, len)
            _       -> return quad

    tryMpart s hdr Nothing     = do
        -- We don't know the size of the file.
        -- Content-Length cannot help.
        -- Let's obtain it first.
        fileSize <- fromIntegral . P.fileSize <$> P.getFileStatus path
        -- Then, let's manipulate Range in HTTP request.
        return $ tryRange s hdr mRange fileSize
    -- We respect Status.
    -- The validity of Status's value is Applications's responsibility.
    tryMpart s hdr (Just part) = return (s, hdr', beg, len)
      where
        beg = filePartOffset part
        len = filePartByteCount part
        hdr' = addRange mContentRange
        addRange Nothing = addContentRange len beg (beg + len - 1) hdr
        addRange _       = hdr

    tryRange s hdr Nothing      fileSize = (s, hdr, 0, fileSize)
    tryRange s hdr (Just range) fileSize = case mhrange of
        Nothing     -> (s, hdr, 0, fileSize)
        -- FIXME: original code checks if s is 200.
        Just hrange -> case hrange of
            H.ByteRangeFrom   from    -> fromRange hdr fileSize from (fileSize - 1)
            H.ByteRangeFromTo from to -> fromRange hdr fileSize from to
            H.ByteRangeSuffix count   -> fromRange hdr fileSize (fileSize - count) (fileSize - 1)
      where
        mhrange = parseByteRanges range >>= listToMaybe

    fromRange hdr total from to = (H.status206
                                  ,addContentRange total from to hdr
                                  ,from
                                  ,to - from + 1)

----------------------------------------------------------------

sendResponse :: Connection
             -> InternalInfo
             -> (forall a. IO a -> IO a) -- ^ Restore masking state.
             -> Request -- ^ HTTP request.
             -> IndexedHeader -- ^ Indexed header of HTTP request.
             -> Response -- ^ HTTP response including status code and response header.
             -> IO Bool -- ^ Returing True if the connection is persistent.

----------------------------------------------------------------

sendResponse conn ii restore req reqidxhdr (ResponseFile s0 hs0 path mpart) =
    restore $ fileRange reqidxhdr rspidxhdr s0 hs path mpart >>= sendResponseEither
  where
    hs = addAcceptRanges hs0
    th = threadHandle ii
    rspidxhdr = indexResponseHeader hs0
    sendResponseEither (Right (s, lengthyHeaders, beg, len))
      | hasBody s req = liftIO $ do
          lheader <- composeHeader version s lengthyHeaders
          connSendFile conn path beg len (T.tickle th) [lheader]
          T.tickle th
          return isPersist
      | otherwise = liftIO $ do
          composeHeader version s hs >>= connSendAll conn
          T.tickle th
          return isPersist -- FIXME isKeepAlive?
      where
        version = httpVersion req
        (isPersist,_) = infoFromRequest req reqidxhdr

    sendResponseEither (Left (SomeException _)) =
        sendResponse conn ii restore req reqidxhdr notFound
      where
        notFound = responseLBS H.status404 [(H.hContentType, "text/plain")] "File not found"

----------------------------------------------------------------

sendResponse conn ii restore req reqidxhdr (ResponseBuilder s hs b)
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
  | otherwise = restore $ do
      composeHeader version s hs >>= connSendAll conn
      T.tickle th
      return isPersist
  where
    th = threadHandle ii
    version = httpVersion req
    reqinfo@(isPersist,_) = infoFromRequest req reqidxhdr
    (isKeepAlive, needsChunked) = infoFromResponse hs reqinfo

----------------------------------------------------------------

sendResponse conn ii restore req reqidxhdr (ResponseSource s hs withBodyFlush)
  | hasBody s req = withBodyFlush $ \bodyFlush -> restore $ do
      header <- liftIO $ composeHeaderBuilder version s hs needsChunked
      let src = CL.sourceList [header] `mappend` cbody bodyFlush
      src $$ builderToByteString =$ connSink conn th
      return isKeepAlive
  | otherwise = withBodyFlush $ \_bodyFlush -> restore $ do -- make sure any cleanup is called
      composeHeader version s hs >>= connSendAll conn
      T.tickle th
      return isPersist
  where
    th = threadHandle ii
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
    version = httpVersion req
    reqinfo@(isPersist,_) = infoFromRequest req reqidxhdr
    (isKeepAlive, needsChunked) = infoFromResponse hs reqinfo

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

infoFromRequest :: Request -> IndexedHeader -> (Bool,Bool)
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

infoFromResponse :: H.ResponseHeaders -> (Bool,Bool) -> (Bool,Bool)
infoFromResponse hs (isPersist,isChunked) = (isKeepAlive, needsChunked)
  where
    needsChunked = isChunked && not hasLength
    isKeepAlive = isPersist && (isChunked || hasLength)
    hasLength = isJust $ checkLength hs

checkLength :: H.ResponseHeaders -> Maybe ByteString
checkLength = lookup H.hContentLength

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

addContentLength :: Integer -> H.ResponseHeaders -> H.ResponseHeaders
addContentLength cl hdrs = (H.hContentLength, B.pack $ show cl) : hdrs

addAcceptRanges :: H.ResponseHeaders -> H.ResponseHeaders
addAcceptRanges hdrs = (hAcceptRanges, "bytes") : hdrs

addTransferEncoding :: H.ResponseHeaders -> H.ResponseHeaders
addTransferEncoding hdrs = (hTransferEncoding, "chunked") : hdrs

addServer :: H.ResponseHeaders -> H.ResponseHeaders
addServer hdrs = case lookup hServer hdrs of
    Nothing -> (hServer, defaultServerValue) : hdrs
    Just _  -> hdrs

addContentRange :: Integer -> Integer -> Integer -> H.ResponseHeaders -> H.ResponseHeaders
addContentRange total from to hdrs = (hContentRange, range) : hdrs
  where
    range = B.pack
      -- building with ShowS
      $ 'b' : 'y': 't' : 'e' : 's' : ' '
      : showInt from
      ( '-'
      : showInt to
      ( '/'
      : showInt total ""))

----------------------------------------------------------------

composeHeader :: H.HttpVersion
              -> H.Status
              -> H.ResponseHeaders
              -> IO ByteString
composeHeader version s hs =
    RH.composeHeader version s $ addServer hs

composeHeaderBuilder :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> IO Builder
composeHeaderBuilder ver s hs True =
    fromByteString <$> composeHeader ver s (addTransferEncoding hs)
composeHeaderBuilder ver s hs False =
    fromByteString <$> composeHeader ver s hs
