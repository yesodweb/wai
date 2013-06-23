{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

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
import Data.Conduit.Blaze (builderToByteString)
import qualified Data.Conduit.List as CL
import Data.Maybe (isJust, listToMaybe)
import Data.Monoid (mappend)
import qualified Network.HTTP.Types as H
import Network.Wai
import Network.Wai.Handler.Warp.ReadInt
import Network.Wai.Handler.Warp.Settings
import qualified Network.Wai.Handler.Warp.ResponseHeader as RH
import qualified Network.Wai.Handler.Warp.Timeout as T
import Network.Wai.Handler.Warp.Types
import qualified System.PosixCompat.Files as P
import Network.HTTP.Attoparsec (parseByteRanges)
import Numeric (showInt)

----------------------------------------------------------------
----------------------------------------------------------------

sendResponse :: Settings
             -> Cleaner -> Request -> Connection -> Response
             -> ResourceT IO Bool

----------------------------------------------------------------

sendResponse settings cleaner req conn (ResponseFile s0 hs0 path mpart0) =
    headerAndLength >>= sendResponse'
  where
    hs = addAccept hs0
    th = threadHandle cleaner
    headerAndLength = liftIO . try $ do
        (hadLength, cl) <-
            case readInt <$> checkLength hs of
                Just cl -> return (True, cl)
                Nothing ->
                    case mpart0 of
                        Just part -> return (False, fromIntegral $ filePartByteCount part)
                        Nothing -> (False, ) . fromIntegral . P.fileSize
                               <$> P.getFileStatus path
        let (s, addRange, beg, end) =
                case mpart0 of
                    Just part -> (s0, id, filePartOffset part, filePartByteCount part)
                    Nothing ->
                        case lookup H.hRange (requestHeaders req) >>= parseByteRanges >>= listToMaybe of
                            Just range | s0 == H.status200 ->
                                case range of
                                    H.ByteRangeFrom from -> rangeRes cl from (cl - 1)
                                    H.ByteRangeFromTo from to -> rangeRes cl from to
                                    H.ByteRangeSuffix count -> rangeRes cl (cl - count) (cl - 1)
                            _ -> (s0, id, 0, cl)
            hs'
                | hadLength = hs
                | otherwise = addLength end hs
        return (s, addRange hs', beg, end)

    rangeRes cl from to = (H.status206, (("Content-Range", rangeHeader cl from to):), from, to - from + 1)

    rangeHeader total from to = B.pack
      $ 'b' : 'y': 't' : 'e' : 's' : ' '
      : showInt from
      ( '-'
      : showInt to
      ( '/'
      : showInt total ""))

    sendResponse' (Right (s, lengthyHeaders, beg, end))
      | hasBody s req = liftIO $ do
          lheader <- composeHeader settings version s lengthyHeaders
          connSendFile conn path beg end (T.tickle th) [lheader] cleaner
          T.tickle th
          return isPersist
      | otherwise = liftIO $ do
          composeHeader settings version s hs >>= connSendAll conn
          T.tickle th
          return isPersist -- FIXME isKeepAlive?
      where
        version = httpVersion req
        (isPersist,_) = infoFromRequest req

    sendResponse' (Left (_ :: SomeException)) =
        sendResponse settings cleaner req conn notFound
      where
        notFound = responseLBS H.status404 [(H.hContentType, "text/plain")] "File not found"

----------------------------------------------------------------

sendResponse settings cleaner req conn (ResponseBuilder s hs b)
  | hasBody s req = liftIO $ do
      header <- composeHeaderBuilder settings version s hs needsChunked
      let body
            | needsChunked = header `mappend` chunkedTransferEncoding b
                                    `mappend` chunkedTransferTerminator
            | otherwise    = header `mappend` b
      flip toByteStringIO body $ \bs -> do
          connSendAll conn bs
          T.tickle th
      return isKeepAlive
  | otherwise = liftIO $ do
      composeHeader settings version s hs >>= connSendAll conn
      T.tickle th
      return isPersist
  where
    th = threadHandle cleaner
    version = httpVersion req
    reqinfo@(isPersist,_) = infoFromRequest req
    (isKeepAlive, needsChunked) = infoFromResponse hs reqinfo

----------------------------------------------------------------

sendResponse settings cleaner req conn (ResponseSource s hs bodyFlush)
  | hasBody s req = do
      header <- liftIO $ composeHeaderBuilder settings version s hs needsChunked
      let src = CL.sourceList [header] `mappend` cbody
      src $$ builderToByteString =$ connSink conn th
      return isKeepAlive
  | otherwise = liftIO $ do
      composeHeader settings version s hs >>= connSendAll conn
      T.tickle th
      return isPersist
  where
    th = threadHandle cleaner
    body =
           mapOutput (\x -> case x of
                    Flush -> flush
                    Chunk builder -> builder)
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

addAccept :: H.ResponseHeaders -> H.ResponseHeaders
addAccept = (("Accept-Ranges", "bytes"):)

addEncodingHeader :: H.ResponseHeaders -> H.ResponseHeaders
addEncodingHeader hdrs = (hTransferEncoding, "chunked") : hdrs

addServerHeader :: Settings -> H.ResponseHeaders -> H.ResponseHeaders
addServerHeader settings hdrs = case lookup hServer hdrs of
    Nothing -> warpVersionHeader settings : hdrs
    Just _  -> hdrs

warpVersionHeader :: Settings -> H.Header
warpVersionHeader settings = (hServer, settingsServerName settings)

----------------------------------------------------------------

composeHeader :: Settings
              -> H.HttpVersion
              -> H.Status
              -> H.ResponseHeaders
              -> IO ByteString
composeHeader settings version s hs =
    RH.composeHeader version s
  $ addServerHeader settings hs

composeHeaderBuilder :: Settings -> H.HttpVersion -> H.Status -> H.ResponseHeaders -> Bool -> IO Builder
composeHeaderBuilder settings ver s hs True =
    fromByteString <$> composeHeader settings ver s (addEncodingHeader hs)
composeHeaderBuilder settings ver s hs False =
    fromByteString <$> composeHeader settings ver s hs
