{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.Response where

import Blaze.ByteString.Builder (copyByteString, Builder, toLazyByteString, toByteStringIO, flush)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)
import Blaze.ByteString.Builder.HTTP (chunkedTransferEncoding, chunkedTransferTerminator)
import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import Data.Conduit
import Data.Conduit.Blaze (builderToByteString)
import qualified Data.Conduit.List as CL
import Data.List (foldl')
import Data.Maybe (isJust)
import Data.Monoid (mappend)
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp.ReadInt
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
            return $ (addLength cl hs, cl)
        (Nothing, Just part) -> do
            let cl = fromIntegral $ filePartByteCount part
            return $ Right $ (addLength cl hs, cl)

    sendResponse' (Right (lengthyHeaders, cl))
      | hasBody s req = liftIO $ do
          let (beg,end) = case mpart of
                  Nothing  -> (0,cl)
                  Just prt -> (filePartOffset prt, filePartByteCount prt)
          connSendFile conn fp beg end (T.tickle th) headers'
          T.tickle th
          return isPersist
      | otherwise = liftIO $ do
          sendHeader conn $ headers version s hs False -- FIXME
          T.tickle th
          return isPersist -- FIXME isKeepAlive?
      where
        version = httpVersion req
        (isPersist,_) = infoFromRequest req
        headers' = L.toChunks . toLazyByteString $ headers version s lengthyHeaders False

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
      sendHeader conn $ headers' False
      T.tickle th
      return isPersist
  where
    headers' = headers version s hs
    body = if needsChunked then
               headers' needsChunked
                 `mappend` chunkedTransferEncoding b
                 `mappend` chunkedTransferTerminator
             else
               headers' False `mappend` b
    version = httpVersion req
    reqinfo@(isPersist,_) = infoFromRequest req
    (isKeepAlive, needsChunked) = infoFromResponse hs reqinfo

----------------------------------------------------------------

sendResponse th req conn (ResponseSource s hs bodyFlush)
  | hasBody s req = do
      let src = CL.sourceList [headers' needsChunked] `mappend`
                (if needsChunked then body $= chunk else body)
      src $$ builderToByteString =$ connSink conn th
      return $ isKeepAlive
  | otherwise = liftIO $ do
      sendHeader conn $ headers' False
      T.tickle th
      return isPersist
  where
    body = mapOutput (\x -> case x of
                    Flush -> flush
                    Chunk builder -> builder) bodyFlush
    headers' = headers version s hs
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

sendHeader :: Connection -> Builder -> IO ()
sendHeader conn = connSendMany conn . L.toChunks . toLazyByteString

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

httpBuilder, spaceBuilder, newlineBuilder, transferEncodingBuilder
           , colonSpaceBuilder :: Builder
httpBuilder = copyByteString "HTTP/"
spaceBuilder = fromChar ' '
newlineBuilder = copyByteString "\r\n"
transferEncodingBuilder = copyByteString "Transfer-Encoding: chunked\r\n\r\n"
colonSpaceBuilder = copyByteString ": "

headers :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> Builder
headers !httpversion !status !responseHeaders !isChunked = {-# SCC "headers" #-}
    let !start = httpBuilder
                `mappend` copyByteString
                            (case httpversion of
                                H.HttpVersion 1 1 -> "1.1"
                                _ -> "1.0")
                `mappend` spaceBuilder
                `mappend` fromShow (H.statusCode status)
                `mappend` spaceBuilder
                `mappend` copyByteString (H.statusMessage status)
                `mappend` newlineBuilder
        !start' = foldl' responseHeaderToBuilder start (addServerHeader responseHeaders)
        !end = if isChunked then transferEncodingBuilder else newlineBuilder
    in start' `mappend` end

responseHeaderToBuilder :: Builder -> H.Header -> Builder
responseHeaderToBuilder b (x, y) = b
  `mappend` copyByteString (CI.original x)
  `mappend` colonSpaceBuilder
  `mappend` copyByteString y
  `mappend` newlineBuilder

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
