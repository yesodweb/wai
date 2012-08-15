{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.Response (
    sendResponse
  ) where

import Blaze.ByteString.Builder (fromByteString, Builder, toByteStringIO, flush)
import Blaze.ByteString.Builder.HTTP (chunkedTransferEncoding, chunkedTransferTerminator)
import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.CaseInsensitive as CI
import Data.Conduit
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

sendResponse :: T.Handle
             -> Request -> Connection -> Response -> ResourceT IO Bool

----------------------------------------------------------------

sendResponse th req conn (ResponseFile s hs fp mpart) =
    headerAndLength >>= sendResponse'
  where
    headerAndLength = case (readInt <$> checkLength hs, mpart) of
        (Just cl, _)         -> return $ Right (hs, cl)
        (Nothing, Nothing)   -> liftIO . try $ do
            cl <- fromIntegral . P.fileSize <$> P.getFileStatus fp
            return (addLength cl hs, cl)
        (Nothing, Just part) -> do
            let cl = fromIntegral $ filePartByteCount part
            return $ Right (addLength cl hs, cl)

    sendResponse' (Right (lengthyHeaders, cl))
      | hasBody s req = liftIO $ do
          connSendFile conn fp beg end (T.tickle th) [lheader]
          T.tickle th
          return isPersist
      | otherwise = liftIO $ do
          connSendAll conn $ composeHeader version s hs
          T.tickle th
          return isPersist -- FIXME isKeepAlive?
      where
        (beg,end) = case mpart of
            Nothing  -> (0,cl)
            Just prt -> (filePartOffset prt, filePartByteCount prt)
        lheader = composeHeader version s lengthyHeaders
        version = httpVersion req
        (isPersist,_) = infoFromRequest req

    sendResponse' (Left (_ :: SomeException)) =
        sendResponse th req conn notFound
      where
        notFound = responseLBS H.status404 [(H.hContentType, "text/plain")] "File not found"

----------------------------------------------------------------

sendResponse th req conn (ResponseBuilder s hs b)
  | hasBody s req = liftIO $ do
      flip toByteStringIO body $ \bs -> do
          connSendAll conn bs
          T.tickle th
      return isKeepAlive
  | otherwise = liftIO $ do
      connSendAll conn $ composeHeader version s hs
      T.tickle th
      return isPersist
  where
    header = composeHeaderBuilder version s hs needsChunked
    body
      | needsChunked = header `mappend` chunkedTransferEncoding b
                              `mappend` chunkedTransferTerminator
      | otherwise    = header `mappend` b
    version = httpVersion req
    reqinfo@(isPersist,_) = infoFromRequest req
    (isKeepAlive, needsChunked) = infoFromResponse hs reqinfo

----------------------------------------------------------------

sendResponse th req conn (ResponseSource s hs bodyFlush)
  | hasBody s req = do
      let src = CL.sourceList [header] `mappend` cbody
      src $$ builderToByteString =$ connSink conn th
      return isKeepAlive
  | otherwise = liftIO $ do
      connSendAll conn $ composeHeader version s hs
      T.tickle th
      return isPersist
  where
    header = composeHeaderBuilder version s hs needsChunked
    body = mapOutput (\x -> case x of
                    Flush -> flush
                    Chunk builder -> builder) bodyFlush
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
connSink :: Connection -> T.Handle -> Sink B.ByteString (ResourceT IO) ()
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

checkLength :: H.ResponseHeaders -> Maybe B.ByteString
checkLength = lookup H.hContentLength

----------------------------------------------------------------

hasBody :: H.Status -> Request -> Bool
hasBody s req = s /= H.Status 204 ""
             && s /= H.status304
             && H.statusCode s >= 200
             && requestMethod req /= H.methodHead

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

composeHeader :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> B.ByteString
composeHeader version s hs = RH.composeHeader version s (addServerHeader hs)

composeHeaderBuilder :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> Builder
composeHeaderBuilder ver s hs True =
    fromByteString $ composeHeader ver s (addEncodingHeader hs)
composeHeaderBuilder ver s hs False =
    fromByteString $ composeHeader ver s hs
