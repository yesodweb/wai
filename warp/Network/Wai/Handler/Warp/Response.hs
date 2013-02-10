{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Response (
    sendResponse
  ) where

import Blaze.ByteString.Builder (fromByteString, Builder, toByteStringIO, flush)
import Blaze.ByteString.Builder.HTTP (chunkedTransferEncoding, chunkedTransferTerminator)
import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B (pack)
import qualified Data.CaseInsensitive as CI
import Data.Conduit
#if MIN_VERSION_conduit(1, 0, 0)
import Data.Conduit.Internal (mapOutput, SourceM (..))
#endif
import Data.Conduit.Blaze (builderToByteString)
import qualified Data.Conduit.List as CL
import Data.Maybe (isJust)
import Data.Monoid (mappend)
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp.ReadInt
import qualified Network.Wai.Handler.Warp.ResponseHeader as RH
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai.Handler.Warp.Types
import qualified System.PosixCompat.Files as P

----------------------------------------------------------------
----------------------------------------------------------------

sendResponse :: Cleaner -> Request -> Connection -> Response
             -> ResourceT IO Bool

----------------------------------------------------------------

sendResponse cleaner req conn (ResponseFile s hs path mpart) =
    headerAndLength >>= sendResponse'
  where
    th = threadHandle cleaner
    headerAndLength = case (readInt <$> checkLength hs, mpart) of
        (Just cl, _)         -> return $ Right (hs, cl)
        (Nothing, Nothing)   -> liftIO . try $ do
            cl <- fromIntegral . P.fileSize <$> P.getFileStatus path
            return (addLength cl hs, cl)
        (Nothing, Just part) -> do
            let cl = fromIntegral $ filePartByteCount part
            return $ Right (addLength cl hs, cl)

    sendResponse' (Right (lengthyHeaders, cl))
      | hasBody s req = liftIO $ do
          lheader <- composeHeader version s lengthyHeaders
          connSendFile conn path beg end (T.tickle th) [lheader] cleaner
          T.tickle th
          return isPersist
      | otherwise = liftIO $ do
          composeHeader version s hs >>= connSendAll conn
          T.tickle th
          return isPersist -- FIXME isKeepAlive?
      where
        (beg,end) = case mpart of
            Nothing  -> (0,cl)
            Just prt -> (filePartOffset prt, filePartByteCount prt)
        version = httpVersion req
        (isPersist,_) = infoFromRequest req

    sendResponse' (Left (_ :: SomeException)) =
        sendResponse cleaner req conn notFound
      where
        notFound = responseLBS H.status404 [(H.hContentType, "text/plain")] "File not found"

----------------------------------------------------------------

sendResponse cleaner req conn (ResponseBuilder s hs b)
  | hasBody s req = liftIO $ do
      header <- composeHeaderBuilder version s hs needsChunked
      let body
            | needsChunked = header `mappend` chunkedTransferEncoding b
                                    `mappend` chunkedTransferTerminator
            | otherwise    = header `mappend` b
      flip toByteStringIO body $ \bs -> do
          connSendAll conn bs
          T.tickle th
      return isKeepAlive
  | otherwise = liftIO $ do
      composeHeader version s hs >>= connSendAll conn
      T.tickle th
      return isPersist
  where
    th = threadHandle cleaner
    version = httpVersion req
    reqinfo@(isPersist,_) = infoFromRequest req
    (isKeepAlive, needsChunked) = infoFromResponse hs reqinfo

----------------------------------------------------------------

sendResponse cleaner req conn (ResponseSource s hs bodyFlush)
  | hasBody s req = do
      header <- liftIO $ composeHeaderBuilder version s hs needsChunked
      let src = CL.sourceList [header] `mappend` cbody
      src $$ builderToByteString =$ connSink conn th
      return isKeepAlive
  | otherwise = liftIO $ do
      composeHeader version s hs >>= connSendAll conn
      T.tickle th
      return isPersist
  where
    th = threadHandle cleaner
    body =
#if MIN_VERSION_conduit(1, 0, 0)
           SourceM $
#endif
           mapOutput (\x -> case x of
                    Flush -> flush
                    Chunk builder -> builder)
#if MIN_VERSION_conduit(1, 0, 0)
         $ unSourceM
#endif
           bodyFlush
    cbody = if needsChunked then body $= chunk else body
    -- FIXME perhaps alloca a buffer per thread and reuse that in all
    -- functions below. Should lessen greatly the GC burden (I hope)
    chunk :: Conduit Builder (ResourceT IO) Builder
    chunk = await >>= maybe (yield chunkedTransferTerminator) (\x -> yield (chunkedTransferEncoding x) >> chunk)
    version = httpVersion req
    reqinfo@(isPersist,_) = infoFromRequest req
    (isKeepAlive, needsChunked) = infoFromResponse hs reqinfo

----------------------------------------------------------------
----------------------------------------------------------------

-- | Use 'connSendAll' to send this data while respecting timeout rules.
connSink :: Connection -> T.Handle -> Sink ByteString (ResourceT IO) ()
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

infoFromRequest :: Request -> (Bool,Bool)
infoFromRequest req = (checkPersist req, checkChunk req)

checkPersist :: Request -> Bool
checkPersist req
    | ver == H.http11 = checkPersist11 conn
    | otherwise       = checkPersist10 conn
  where
    ver = httpVersion req
    conn = lookup H.hConnection $ requestHeaders req
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

addLength :: Integer -> H.ResponseHeaders -> H.ResponseHeaders
addLength cl hdrs = (H.hContentLength, B.pack $ show cl) : hdrs

addEncodingHeader :: H.ResponseHeaders -> H.ResponseHeaders
addEncodingHeader hdrs = (hTransferEncoding, "chunked") : hdrs

addServerHeader :: H.ResponseHeaders -> H.ResponseHeaders
addServerHeader hdrs = case lookup hServer hdrs of
    Nothing -> warpVersionHeader : hdrs
    Just _  -> hdrs

warpVersionHeader :: H.Header
warpVersionHeader = (hServer, ver)
  where
    ver = B.pack $ "Warp/" ++ warpVersion

----------------------------------------------------------------

composeHeader :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> IO ByteString
composeHeader version s hs = RH.composeHeader version s (addServerHeader hs)

composeHeaderBuilder :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> IO Builder
composeHeaderBuilder ver s hs True =
    fromByteString <$> composeHeader ver s (addEncodingHeader hs)
composeHeaderBuilder ver s hs False =
    fromByteString <$> composeHeader ver s hs
