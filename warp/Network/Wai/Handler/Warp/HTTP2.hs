{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Warp.HTTP2 (
    http2
  , http2server
  ) where

import qualified Data.IORef as I
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

http2 :: S.Settings
      -> InternalInfo
      -> Connection
      -> Transport
      -> SockAddr
      -> (BufSize -> IO ByteString)
      -> (ByteString -> IO ())
      -> Application
      -> IO ()
http2 settings ii conn transport addr readN send app =
    H2.run conf $ http2server settings ii transport addr app
  where
    conf = H2.Config {
        confWriteBuffer       = connWriteBuffer conn
      , confBufferSize        = connBufferSize conn
      , confSendAll           = send
      , confReadN             = readN
      , confPositionReadMaker = pReadMaker ii
      }

-- | Converting WAI application to the server type of http2 library.
--
-- Since 3.3.11
http2server :: S.Settings
            -> InternalInfo
            -> Transport
            -> SockAddr
            -> Application
            -> H2.Server
http2server settings ii transport addr app h2req0 aux0 response = do
    req <- toWAIRequest h2req0 aux0
    ref <- I.newIORef Nothing
    eResponseReceived <- E.try $ app req $ \rsp -> do
        (h2rsp,st) <- fromResponse settings ii req rsp
        pps <- fromPushPromises ii req
        I.writeIORef ref $ Just (h2rsp, pps, st)
        _ <- response h2rsp pps
        return ResponseReceived
    case eResponseReceived of
      Right ResponseReceived -> do
          Just (h2rsp, pps, st) <- I.readIORef ref
          let msiz = fromIntegral <$> H2.responseBodySize h2rsp
          logResponse req st msiz
          mapM_ (logPushPromise req) pps
      Left e@(E.SomeException _)
        -- killed by the local worker manager
        | Just E.ThreadKilled  <- E.fromException e -> return ()
        -- killed by the local timeout manager
        | Just T.TimeoutThread <- E.fromException e -> return ()
        | otherwise -> do
            S.settingsOnException settings (Just req) e
            let ersp = S.settingsOnExceptionResponse settings e
                st = responseStatus ersp
            (h2rsp',_) <- fromResponse settings ii req ersp
            let msiz = fromIntegral <$> H2.responseBodySize h2rsp'
            _ <- response h2rsp' []
            logResponse req st msiz
    return ()
  where
    toWAIRequest h2req aux = toRequest ii settings addr hdr bdylen bdy th transport
      where
        !hdr = H2.requestHeaders h2req
        !bdy = H2.getRequestBodyChunk h2req
        !bdylen = H2.requestBodySize h2req
        !th = H2.auxTimeHandle aux

    logResponse = S.settingsLogger settings

    logPushPromise req pp = logger req path siz
      where
        !logger = S.settingsServerPushLogger settings
        !path = H2.promiseRequestPath pp
        !siz = case H2.responseBodySize $ H2.promiseResponse pp of
            Nothing -> 0
            Just s  -> fromIntegral s
