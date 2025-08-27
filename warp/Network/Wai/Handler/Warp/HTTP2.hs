{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Warp.HTTP2 (
    http2,
    http2server,
) where

import qualified Control.Exception as E
import qualified Data.ByteString as BS
import Data.IORef (readIORef)
import qualified Data.IORef as I
import GHC.Conc.Sync (labelThread, myThreadId)
import qualified Network.HTTP2.Frame as H2
import qualified Network.HTTP2.Server as H2
import Network.Socket (SockAddr)
import Network.Socket.BufferPool
import Network.Wai
import Network.Wai.Internal (ResponseReceived (..))
import qualified System.TimeManager as T

import Network.Wai.Handler.Warp.HTTP2.File
import Network.Wai.Handler.Warp.HTTP2.PushPromise
import Network.Wai.Handler.Warp.HTTP2.Request
import Network.Wai.Handler.Warp.HTTP2.Response
import Network.Wai.Handler.Warp.Imports
import qualified Network.Wai.Handler.Warp.Settings as S
import Network.Wai.Handler.Warp.Types

----------------------------------------------------------------

http2
    :: S.Settings
    -> InternalInfo
    -> Connection
    -> Transport
    -> Application
    -> SockAddr
    -> T.Handle
    -> ByteString
    -> IO ()
http2 settings ii conn transport app peersa th bs = do
    rawRecvN <- makeRecvN bs $ connRecv conn
    writeBuffer <- readIORef $ connWriteBuffer conn
    -- This thread becomes the sender in http2 library.
    -- In the case of event source, one request comes and one
    -- worker gets busy. But it is likely that the receiver does
    -- not receive any data at all while the sender is sending
    -- output data from the worker. It's not good enough to tickle
    -- the time handler in the receiver only. So, we should tickle
    -- the time handler in both the receiver and the sender.
    let recvN = wrappedRecvN th (S.settingsSlowlorisSize settings) rawRecvN
        sendBS x = connSendAll conn x >> T.tickle th
        conf =
            H2.Config
                { confWriteBuffer = bufBuffer writeBuffer
                , confBufferSize = bufSize writeBuffer
                , confSendAll = sendBS
                , confReadN = recvN
                , confPositionReadMaker = pReadMaker ii
                , confTimeoutManager = timeoutManager ii
#if MIN_VERSION_http2(4,2,0)
                , confMySockAddr = connMySockAddr conn
                , confPeerSockAddr = peersa
#endif
                }
    checkTLS
    setConnHTTP2 conn True
    H2.run H2.defaultServerConfig conf $
        http2server "Warp HTTP/2" settings ii transport peersa app
  where
    checkTLS = case transport of
        TCP -> return () -- direct
        tls -> unless (tls12orLater tls) $ goaway conn H2.InadequateSecurity "Weak TLS"
    tls12orLater tls = tlsMajorVersion tls == 3 && tlsMinorVersion tls >= 3

-- | Converting WAI application to the server type of http2 library.
--
-- Since 3.3.11
http2server
    :: String
    -> S.Settings
    -> InternalInfo
    -> Transport
    -> SockAddr
    -> Application
    -> H2.Server
http2server label settings ii transport addr app h2req0 aux0 response = do
    tid <- myThreadId
    labelThread tid (label ++ " http2server " ++ show addr)
    req <- toWAIRequest h2req0 aux0
    ref <- I.newIORef Nothing
    eResponseReceived <- E.try $ app req $ \rsp -> do
        (h2rsp, st, hasBody) <- fromResponse settings ii req rsp
        pps <- if hasBody then fromPushPromises ii req else return []
        I.writeIORef ref $ Just (h2rsp, pps, st)
        _ <- response h2rsp pps
        return ResponseReceived
    case eResponseReceived of
        Right ResponseReceived -> do
            Just (h2rsp, pps, st) <- I.readIORef ref
            let msiz = fromIntegral <$> H2.responseBodySize h2rsp
            logResponse req st msiz
            mapM_ (logPushPromise req) pps
        Left e
          | isAsyncException e -> E.throwIO e
          | otherwise -> do
            S.settingsOnException settings (Just req) e
            let ersp = S.settingsOnExceptionResponse settings e
                st = responseStatus ersp
            (h2rsp', _, _) <- fromResponse settings ii req ersp
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
            Just s -> fromIntegral s

wrappedRecvN
    :: T.Handle -> Int -> (BufSize -> IO ByteString) -> (BufSize -> IO ByteString)
wrappedRecvN th slowlorisSize readN bufsize = do
    bs <- E.handle handler $ readN bufsize
    -- TODO: think about the slowloris protection in HTTP2: current code
    -- might open a slow-loris attack vector. Rather than timing we should
    -- consider limiting the per-client connections assuming that in HTTP2
    -- we should allow only few connections per host (real-world
    -- deployments with large NATs may be trickier).
    when
        (BS.length bs > 0 && BS.length bs >= slowlorisSize || bufsize <= slowlorisSize) $
        T.tickle th
    return bs
  where
    handler :: E.SomeException -> IO ByteString
    handler = throughAsync (return "")

-- connClose must not be called here since Run:fork calls it
goaway :: Connection -> H2.ErrorCode -> ByteString -> IO ()
goaway Connection{..} etype debugmsg = connSendAll bytestream
  where
    einfo = H2.encodeInfo id 0
    frame = H2.GoAwayFrame 0 etype debugmsg
    bytestream = H2.encodeFrame einfo frame
