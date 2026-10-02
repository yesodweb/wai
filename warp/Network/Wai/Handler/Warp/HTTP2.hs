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
import Network.HTTP.Semantics (InpObj (..))
import qualified Network.HTTP.Semantics.Server.Internal as H2I
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
import Network.Wai.Handler.Warp.Watchdog (Watchdog, rxTick, waitingForPeer)

-- Early Hints wiring needs both the http-semantics 'auxSendInformational' field
-- (0.4.1) and the http2 sender support that actually emits it (5.4.2).
#define HAS_EARLY_HINTS_SUPPORT (MIN_VERSION_http_semantics(0,4,1) && MIN_VERSION_http2(5,4,2))

#if HAS_EARLY_HINTS_SUPPORT
import qualified Network.HTTP.Types as H
#endif

----------------------------------------------------------------

-- | Serve an HTTP/2 connection. Match TLS fields on their constructor so the
-- version check stays total as transports are added; QUIC uses the separate
-- HTTP/3 entry point and establishes TLS 1.3 in that transport.
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
http2 settings ii conn transport app peersa _th bs = do
    rawRecvN <- makeRecvN bs $ connRecv conn
    writeBuffer <- readIORef $ connWriteBuffer conn
    -- This thread becomes the sender in http2 library.
    --
    -- The connection is supervised by its watchdog (see 'Run.fork') as
    -- HTTP/1.1 is: writes are reported by 'connSendAll' itself, running
    -- streams by 'connAppsInProgress', and request bodies being waited
    -- for by 'watchRequestBody'. The timers of the http2 library are
    -- disabled by giving it the dummy 'T.defaultManager', which turns
    -- every 'T.Handle' it creates into 'T.emptyHandle'.
    let wd = connWatchdog conn
        recvN = wrappedRecvN wd (S.settingsSlowlorisSize settings) rawRecvN
        sendBS = connSendAll conn
        conf =
            H2.defaultConfig
                { H2.confWriteBuffer = bufBuffer writeBuffer
                , H2.confBufferSize = bufSize writeBuffer
                , H2.confSendAll = sendBS
                , H2.confReadN = recvN
                , H2.confPositionReadMaker = pReadMaker ii
                , H2.confTimeoutManager = T.defaultManager
                , H2.confMySockAddr = connMySockAddr conn
                , H2.confPeerSockAddr = peersa
                , H2.confReadNTimeout = True
                }
    checkTLS
    setConnHTTP2 conn True
    H2.run H2.defaultServerConfig conf $
        watchRequestBody wd $
            http2server "Warp HTTP/2" settings ii transport peersa app
  where
    checkTLS = case transport of
        TCP -> return () -- direct
        TLS{tlsMajorVersion = major, tlsMinorVersion = minor} ->
            unless (major == 3 && minor >= 3) $ goaway conn H2.InadequateSecurity "Weak TLS"
        -- QUIC establishes TLS 1.3 itself and calls http2server through HTTP/3.
        QUIC{} -> return ()

-- | Converting WAI application to the server type of http2 library.
--
-- @since 3.3.11
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
    req0 <- toWAIRequest h2req0 aux0
#if HAS_EARLY_HINTS_SUPPORT
    let req = req0{requestSendEarlyHints = H2.auxSendInformational aux0 (H.mkStatus 103 "Early Hints")}
#else
    let req = req0
#endif
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

-- | Reporting to the watchdog that a stream waits for its request body.
--   While it does, the peer has to make progress.
watchRequestBody :: Watchdog -> H2.Server -> H2.Server
watchRequestBody wd server (H2I.Request inp) =
    server $ H2I.Request inp{inpObjBody = waitingForPeer wd $ inpObjBody inp}

wrappedRecvN
    :: Watchdog -> Int -> (BufSize -> IO ByteString) -> (BufSize -> IO ByteString)
wrappedRecvN wd slowlorisSize readN bufsize = do
    bs <- E.handle handler $ readN bufsize
    -- TODO: think about the slowloris protection in HTTP2: current code
    -- might open a slow-loris attack vector. Rather than timing we should
    -- consider limiting the per-client connections assuming that in HTTP2
    -- we should allow only few connections per host (real-world
    -- deployments with large NATs may be trickier).
    when
        (BS.length bs > 0 && BS.length bs >= slowlorisSize || bufsize <= slowlorisSize)
        $ rxTick wd
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
