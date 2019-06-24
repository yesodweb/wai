{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Warp.HTTP2 (http2) where

import qualified Control.Exception as E
import qualified Network.HTTP2.Server as H2
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Internal (ResponseReceived(..))
import qualified System.TimeManager as T

import Network.Wai.Handler.Warp.HTTP2.File
import Network.Wai.Handler.Warp.HTTP2.PushPromise
import Network.Wai.Handler.Warp.HTTP2.Request
import Network.Wai.Handler.Warp.HTTP2.Response
import Network.Wai.Handler.Warp.Imports
import qualified Network.Wai.Handler.Warp.Settings as S
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

http2 :: Connection -> InternalInfo -> SockAddr -> S.Settings -> (BufSize -> IO ByteString) -> Application -> IO ()
http2 conn ii addr settings readN app =
    H2.run conf http2server
  where
    conf = H2.Config {
        confWriteBuffer       = connWriteBuffer conn
      , confBufferSize        = connBufferSize conn
      , confSendAll           = connSendAll conn
      , confReadN             = readN
      , confPositionReadMaker = pReadMaker ii
      }

    http2server h2req aux response = do
        req <- toWAIRequest h2req aux
        ResponseReceived <- app req $ \rsp -> do
            h2rsp <- fromResponse settings ii req rsp
            pps <- fromPushPromises ii req
            ex <- E.try $ response h2rsp pps
            case ex of
              Right () -> do
                  logResponse h2rsp req
                  mapM_ (logPushPromise req) pps
              Left  e@(E.SomeException _)
                -- killed by the local worker manager
                | Just E.ThreadKilled  <- E.fromException e -> return ()
                -- killed by the local timeout manager
                | Just T.TimeoutThread <- E.fromException e -> return ()
                | otherwise -> S.settingsOnException settings (Just req) e
            return ResponseReceived
        return ()

    toWAIRequest h2req aux = toRequest ii settings addr hdr bdylen bdy th
      where
        !hdr = H2.requestHeaders h2req
        !bdy = H2.getRequestBodyChunk h2req
        !bdylen = H2.requestBodySize h2req
        !th = H2.auxTimeHandle aux

    logResponse h2rsp req = logger req st msiz
      where
        !logger = S.settingsLogger settings
        !st = H2.responseStatus h2rsp
        !msiz = fromIntegral <$> H2.responseBodySize h2rsp

    logPushPromise req pp = logger req path siz
      where
        !logger = S.settingsServerPushLogger settings
        !path = H2.promiseRequestPath pp
        !siz = case H2.responseBodySize $ H2.promiseResponse pp of
            Nothing -> 0
            Just s  -> fromIntegral s
