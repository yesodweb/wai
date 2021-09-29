{-# LANGUAGE OverloadedStrings #-}

-- | WAI handler for HTTP/3 based on QUIC.
module Network.Wai.Handler.WarpQUIC where

import qualified Data.ByteString as BS
import qualified Network.HQ.Server as HQ
import qualified Network.HTTP3.Server as H3
import Network.QUIC
import Network.QUIC.Server as Q
import Network.TLS (cipherID)
import Network.Wai
import Network.Wai.Handler.Warp hiding (run)
import Network.Wai.Handler.Warp.Internal

-- | QUIC server settings.
type QUICSettings = ServerConfig

-- | Running warp with HTTP/3 on QUIC.
runQUIC :: QUICSettings -> Settings -> Application -> IO ()
runQUIC quicsettings settings app = do
    withII settings $ \ii ->
        Q.run quicsettings $ \conn -> do
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
           case malpn of
             Nothing -> return ()
             Just appProto -> do
                 let runX | "h3" `BS.isPrefixOf` appProto = H3.run
                          | otherwise                     = HQ.run
                 runX conn conf $ http2server settings ii transport addr app
