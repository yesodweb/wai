{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-- | WAI handler for HTTP/3 based on QUIC.
module Network.Wai.Handler.WarpQUIC where

import qualified Data.ByteString as BS
import qualified Network.HQ.Server as HQ
import qualified Network.HTTP3.Server as H3
import Network.QUIC
import Network.QUIC.Server as Q
import qualified Control.Exception as E
import Network.Socket (Socket, close)
import Network.TLS (cipherID)
import Network.Wai
import Network.Wai.Handler.Warp hiding (run)
import Network.Wai.Handler.Warp.Internal hiding (Connection)

-- | QUIC server settings.
type QUICSettings = ServerConfig

runQUICSocket :: QUICSettings -> Settings -> Socket -> Application -> IO ()
runQUICSocket quicsettings settings sock app = runQUICSockets quicsettings settings [sock] app

-- | The sockets are closed when the server has stopped, as
--   'Network.Wai.Handler.Warp.runSettingsSocket' closes the one it is given:
--   by then the peers have been told the server is going and the ports are a
--   successor's to take.
runQUICSockets :: QUICSettings -> Settings -> [Socket] -> Application -> IO ()
runQUICSockets quicsettings settings ss app =
    (withII settings $ \ii ->
        Q.runWithSockets ss (stoppableBy settings quicsettings) $
            quicApp settings app ii)
        `E.finally` mapM_ close ss

-- | Running warp with HTTP/3 on QUIC.
runQUIC :: QUICSettings -> Settings -> Application -> IO ()
runQUIC quicsettings settings app =
    withII settings $ \ii ->
        Q.run (stoppableBy settings quicsettings) $ quicApp settings app ii

-- | Handing the action that stops the QUIC server to whoever installs
--   shutdown handlers for this warp.
--
-- The two hooks are the same hook: each is handed the action that stops its
-- server, once, as the server starts, and keeping it is what lets a caller
-- stop a server that has no connection to stop it through.  So an
-- 'Application' served over TCP and over QUIC is stopped the same way on
-- both, by a caller that need not know which it is holding.
--
-- A caller that installs nothing is left as it was: the QUIC server is
-- stopped by whatever its sockets are, being closed.
stoppableBy :: Settings -> QUICSettings -> QUICSettings
#if MIN_VERSION_quic(0,3,15)
stoppableBy settings quicsettings =
    quicsettings
        { Q.scInstallShutdownHandler = settingsInstallShutdownHandler settings
        , Q.scCloseReason = (h3NoError, "server is closing")
        }

-- | H3_NO_ERROR (RFC 9114 Sec 8.1): the connection is being closed and
--   nothing went wrong.  What a stopping server tells its peers, and said in
--   the application protocol because the transport's own NO_ERROR is what a
--   connection whose application has finished normally says.
h3NoError :: ApplicationProtocolError
h3NoError = ApplicationProtocolError 0x100
#else
-- quic before 0.3.15 has no hook to install this on, and no way to stop a
-- server from out here at all.
stoppableBy _ quicsettings = quicsettings
#endif

quicApp
    :: Settings
    -> Application
    -> InternalInfo
    -> Connection
    -> IO ()
quicApp settings app ii conn = do
    info <- getConnectionInfo conn
    mccc <- clientCertificateChain conn
    let addr = remoteSockAddr info
        malpn = alpn info
        transport =
            QUIC
                { quicNegotiatedProtocol = malpn
                , quicChiperID = cipherID $ cipher info
                , quicClientCertificate = mccc
                }
        pread = pReadMaker ii
        timmgr = timeoutManager ii
#if !MIN_VERSION_http3(0,1,0)
        conf = H3.Config H3.defaultHooks pread timmgr
#else
        conf =
            H3.defaultConfig
                { H3.confPositionReadMaker = pread
                , H3.confTimeoutManager = timmgr
                }
#endif
    case malpn of
        Nothing -> return ()
        Just appProto -> do
            let runX
                    | "h3" `BS.isPrefixOf` appProto = H3.run
                    | otherwise = HQ.run
                label
                    | "h3" `BS.isPrefixOf` appProto = "Warp HTTP/3"
                    | otherwise = "Warp HQ"
            runX conn conf $ http2server label settings ii transport addr app
