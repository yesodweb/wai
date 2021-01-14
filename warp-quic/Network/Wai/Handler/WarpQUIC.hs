{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Handler.WarpQUIC where

import qualified Data.ByteString as BS
import qualified Network.HQ.Server as HQ
import qualified Network.HTTP3.Server as H3
import Network.QUIC
import Network.TLS (cipherID)
import Network.Wai
import Network.Wai.Handler.Warp hiding (run)
import Network.Wai.Handler.Warp.Internal

type QUICSettings = ServerConfig

runQUIC :: QUICSettings -> Settings -> Application -> IO ()
runQUIC quicsettings settings app = do
    withII settings $ \ii ->
        runQUICServer quicsettings $ \conn -> do
           info <- getConnectionInfo conn
           mccc <- clientCertificateChain conn
           let addr = remoteSockAddr info
               malpn = alpn info
               transport = QUIC {
                   quicNegotiatedProtocol = malpn
                 , quicChiperID = cipherID $ cipher info
                 , quicClientCertificate = mccc
                 }
               pread = pReadMaker ii
               timmgr = timeoutManager ii
               conf = H3.Config H3.defaultHooks pread timmgr
               Just appProto = malpn
               run | "h3" `BS.isPrefixOf` appProto = H3.run
                   | otherwise                     = HQ.run
           run conn conf $ http2server settings ii transport addr app
