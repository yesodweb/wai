{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.Response where

import Data.List (foldl')
import qualified Data.Conduit.List as CL
import Blaze.ByteString.Builder (copyByteString, Builder, toLazyByteString, toByteStringIO, flush)
import Blaze.ByteString.Builder.Char8 (fromChar, fromShow)
import Blaze.ByteString.Builder.HTTP (chunkedTransferEncoding, chunkedTransferTerminator)
import Control.Exception
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.CaseInsensitive as CI
import Data.Conduit
import Data.Conduit.Blaze (builderToByteString)
import Data.Maybe (isJust)
import Data.Monoid (mappend)
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp.ReadInt
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai.Handler.Warp.Types
import qualified System.PosixCompat.Files as P

sendResponse :: T.Handle
             -> Request -> Connection -> Response -> ResourceT IO Bool
sendResponse th req conn r = sendResponse' r
  where
    version = httpVersion req
    isPersist = checkPersist req
    isChunked' = isChunked version
    needsChunked hs = isChunked' && not (hasLength hs)
    isKeepAlive hs = isPersist && (isChunked' || hasLength hs)
    hasLength hs = isJust $ lookup H.hContentLength hs
    sendHeader = connSendMany conn . L.toChunks . toLazyByteString

    sendResponse' :: Response -> ResourceT IO Bool
    sendResponse' (ResponseFile s hs fp mpart) = do
        eres <-
            case (readInt `fmap` lookup H.hContentLength hs, mpart) of
                (Just cl, _) -> return $ Right (hs, cl)
                (Nothing, Nothing) -> liftIO $ try $ do
                    cl <- P.fileSize `fmap` P.getFileStatus fp
                    return $ addClToHeaders cl
                (Nothing, Just part) -> do
                    let cl = filePartByteCount part
                    return $ Right $ addClToHeaders cl
        case eres of
            Left (_ :: SomeException) -> sendResponse' $ responseLBS
                H.status404
                [(H.hContentType, "text/plain")]
                "File not found"
            Right (lengthyHeaders, cl) -> liftIO $ do
                let headers' = L.toChunks . toLazyByteString $ headers version s lengthyHeaders False
                T.tickle th
                if hasBody s req then do
                    case mpart of
                        Nothing   -> connSendFile conn fp 0 cl (T.tickle th) headers'
                        Just part -> connSendFile conn fp (filePartOffset part) (filePartByteCount part) (T.tickle th) headers'
                    T.tickle th
                    return isPersist
                  else
                    return isPersist
      where
        addClToHeaders cl = ((H.hContentLength, B.pack $ show cl):hs, fromIntegral cl)

    sendResponse' (ResponseBuilder s hs b)
        | hasBody s req = liftIO $ do
              toByteStringIO (\bs -> do
                connSendAll conn bs
                T.tickle th) body
              return (isKeepAlive hs)
        | otherwise = liftIO $ do
            sendHeader $ headers' False
            T.tickle th
            return isPersist
      where
        headers' = headers version s hs
        needsChunked' = needsChunked hs
        body = if needsChunked'
                  then headers' needsChunked'
                       `mappend` chunkedTransferEncoding b
                       `mappend` chunkedTransferTerminator
                  else headers' False `mappend` b

    sendResponse' (ResponseSource s hs bodyFlush)
        | hasBody s req = do
            let src = CL.sourceList [headers' needsChunked'] `mappend`
                      (if needsChunked' then body $= chunk else body)
            src $$ builderToByteString =$ connSink conn th
            return $ isKeepAlive hs
        | otherwise = liftIO $ do
            sendHeader $ headers' False
            T.tickle th
            return isPersist
      where
        body = mapOutput (\x -> case x of
                        Flush -> flush
                        Chunk builder -> builder) bodyFlush
        headers' = headers version s hs
        -- FIXME perhaps alloca a buffer per thread and reuse that in all
        -- functions below. Should lessen greatly the GC burden (I hope)
        needsChunked' = needsChunked hs
        chunk :: Conduit Builder (ResourceT IO) Builder
        chunk = await >>= maybe (yield chunkedTransferTerminator) (\x -> yield (chunkedTransferEncoding x) >> chunk)

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

isChunked :: H.HttpVersion -> Bool
isChunked = (==) H.http11

hasBody :: H.Status -> Request -> Bool
hasBody s req = s /= H.Status 204 "" && s /= H.status304 &&
                H.statusCode s >= 200 && requestMethod req /= "HEAD"

httpBuilder, spaceBuilder, newlineBuilder, transferEncodingBuilder
           , colonSpaceBuilder :: Builder
httpBuilder = copyByteString "HTTP/"
spaceBuilder = fromChar ' '
newlineBuilder = copyByteString "\r\n"
transferEncodingBuilder = copyByteString "Transfer-Encoding: chunked\r\n\r\n"
colonSpaceBuilder = copyByteString ": "

headers :: H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> Builder
headers !httpversion !status !responseHeaders !isChunked' = {-# SCC "headers" #-}
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
        !start' = foldl' responseHeaderToBuilder start (serverHeader responseHeaders)
        !end = if isChunked'
                 then transferEncodingBuilder
                 else newlineBuilder
    in start' `mappend` end

responseHeaderToBuilder :: Builder -> H.Header -> Builder
responseHeaderToBuilder b (x, y) = b
  `mappend` copyByteString (CI.original x)
  `mappend` colonSpaceBuilder
  `mappend` copyByteString y
  `mappend` newlineBuilder

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

serverHeader :: H.RequestHeaders -> H.RequestHeaders
serverHeader hdrs = case lookup key hdrs of
    Nothing  -> server : hdrs
    Just _ -> hdrs
 where
    key = "Server"
    ver = B.pack $ "Warp/" ++ warpVersion
    server = (key, ver)
