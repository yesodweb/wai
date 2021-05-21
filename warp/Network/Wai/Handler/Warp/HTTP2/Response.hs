{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Warp.HTTP2.Response (
    fromResponse
  ) where

import qualified UnliftIO
import qualified Data.ByteString.Builder as BB
import qualified Network.HTTP.Types as H
import qualified Network.HTTP2.Server as H2
import Network.Wai hiding (responseFile, responseBuilder, responseStream)
import Network.Wai.Internal (Response(..))

import Network.Wai.Handler.Warp.File
import Network.Wai.Handler.Warp.HTTP2.Request (getHTTP2Data)
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.Header
import qualified Network.Wai.Handler.Warp.Response as R
import qualified Network.Wai.Handler.Warp.Settings as S
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

fromResponse :: S.Settings -> InternalInfo -> Request -> Response -> IO (H2.Response, H.Status, Bool)
fromResponse settings ii req rsp = do
    date <- getDate ii
    rspst@(h2rsp, st, hasBody) <- case rsp of
      ResponseFile    st rsphdr path mpart -> do
          let rsphdr' = add date svr rsphdr
          responseFile    st rsphdr' isHead path mpart ii reqhdr
      ResponseBuilder st rsphdr builder -> do
          let rsphdr' = add date svr rsphdr
          return $ responseBuilder st rsphdr' isHead builder
      ResponseStream  st rsphdr strmbdy -> do
          let rsphdr' = add date svr rsphdr
          return $ responseStream  st rsphdr' isHead strmbdy
      _ -> error "ResponseRaw is not supported in HTTP/2"
    mh2data <- getHTTP2Data req
    case mh2data of
      Nothing     -> return rspst
      Just h2data -> do
          let !trailers = http2dataTrailers h2data
              !h2rsp' = H2.setResponseTrailersMaker h2rsp trailers
          return (h2rsp', st, hasBody)
  where
    !isHead = requestMethod req == H.methodHead
    !reqhdr = requestHeaders req
    !svr    = S.settingsServerName settings
    add date server rsphdr = R.addAltSvc settings $
        (H.hDate, date) : (H.hServer, server) : rsphdr
    -- fixme: not adding svr if already exists

----------------------------------------------------------------

responseFile :: H.Status -> H.ResponseHeaders -> Bool
             -> FilePath -> Maybe FilePart -> InternalInfo -> H.RequestHeaders
             -> IO (H2.Response, H.Status, Bool)
responseFile st rsphdr _ _ _ _ _
  | noBody st = return $ responseNoBody st rsphdr

responseFile st rsphdr isHead path (Just fp) _ _ =
    return $ responseFile2XX st rsphdr isHead fileSpec
  where
    !off'   = fromIntegral $ filePartOffset fp
    !bytes' = fromIntegral $ filePartByteCount fp
    !fileSpec = H2.FileSpec path off' bytes'

responseFile _ rsphdr isHead path Nothing ii reqhdr = do
    efinfo <- UnliftIO.tryIO $ getFileInfo ii path
    case efinfo of
        Left (_ex :: UnliftIO.IOException) -> return $ response404 rsphdr
        Right finfo -> do
            let reqidx = indexRequestHeader reqhdr
                rspidx = indexResponseHeader rsphdr
            case conditionalRequest finfo rsphdr rspidx reqidx of
                WithoutBody s                -> return $ responseNoBody s rsphdr
                WithBody s rsphdr' off bytes -> do
                    let !off'   = fromIntegral off
                        !bytes' = fromIntegral bytes
                        !fileSpec = H2.FileSpec path off' bytes'
                    return $ responseFile2XX s rsphdr' isHead fileSpec

----------------------------------------------------------------

responseFile2XX :: H.Status -> H.ResponseHeaders -> Bool -> H2.FileSpec -> (H2.Response, H.Status, Bool)
responseFile2XX st rsphdr isHead fileSpec
  | isHead = responseNoBody st rsphdr
  | otherwise = (H2.responseFile st rsphdr fileSpec, st, True)

----------------------------------------------------------------

responseBuilder :: H.Status -> H.ResponseHeaders -> Bool
                -> BB.Builder
                -> (H2.Response, H.Status, Bool)
responseBuilder st rsphdr isHead builder
  | noBody st = responseNoBody st rsphdr
  | isHead    = responseNoBody st rsphdr
  | otherwise = (H2.responseBuilder st rsphdr builder, st, True)

----------------------------------------------------------------

responseStream :: H.Status -> H.ResponseHeaders -> Bool
               -> StreamingBody
               -> (H2.Response, H.Status, Bool)
responseStream st rsphdr isHead strmbdy
  | noBody st = responseNoBody st rsphdr
  | isHead    = responseNoBody st rsphdr
  | otherwise = (H2.responseStreaming st rsphdr strmbdy, st, True)

----------------------------------------------------------------

responseNoBody :: H.Status -> H.ResponseHeaders -> (H2.Response, H.Status, Bool)
responseNoBody st rsphdr = (H2.responseNoBody st rsphdr, st, False)

----------------------------------------------------------------

response404 :: H.ResponseHeaders -> (H2.Response, H.Status, Bool)
response404 rsphdr = (h2rsp, st, True)
  where
    h2rsp = H2.responseBuilder st rsphdr' body
    st = H.notFound404
    !rsphdr' = R.replaceHeader H.hContentType "text/plain; charset=utf-8" rsphdr
    !body = BB.byteString "File not found"

----------------------------------------------------------------

noBody :: H.Status -> Bool
noBody = not . R.hasBody
