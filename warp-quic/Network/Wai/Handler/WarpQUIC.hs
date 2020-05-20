module Network.Wai.Handler.WarpQUIC where

import qualified Network.HTTP3.Server as H3
import Network.QUIC
import Network.TLS (cipherID)
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal

type QUICSettings = ServerConfig

runQUIC :: QUICSettings -> Settings -> Application -> IO ()
runQUIC quicsettings settings app = do
    withII settings $ \ii ->
        runQUICServer quicsettings $ \conn -> do
           info <- getConnectionInfo conn
           mccc <- clientCertificateChain conn
           let addr = remoteSockAddr info
           let transport = QUIC {
                   quicNegotiatedProtocol = alpn info
                 , quicChiperID = cipherID $ cipher info
                 , quicClientCertificate = mccc
                 }
           H3.run conn $ http2server settings ii transport addr app
