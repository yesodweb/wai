{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WarpTLS
    ( TLSSettings (..)
    , runTLS
    ) where

import qualified Network.TLS as TLS
import Network.Wai.Handler.Warp
import Network.Wai
import Network.Socket
import System.IO
import Crypto.Random
import qualified Data.ByteString.Lazy as L
import Data.Conduit.Binary (sourceFileRange)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Exception (bracket, handle, SomeException)
import qualified Network.TLS.Extra as TLSExtra
import qualified Data.Certificate.X509 as X509
import qualified Data.Certificate.PEM as PEM
import qualified Data.ByteString as B
import qualified Data.Certificate.KeyRSA as KeyRSA
import Data.Conduit.Network (bindPort)

data TLSSettings = TLSSettings
    { certFile :: FilePath
    , keyFile :: FilePath
    }

runTLS :: TLSSettings -> Settings -> Application -> IO ()
runTLS tset set app = do
    cert    <- readCertificate $ certFile tset
    pk      <- readPrivateKey $ keyFile tset
    let params = TLS.defaultParams
            { TLS.pWantClientCert = False
            , TLS.pAllowedVersions = [TLS.SSL3,TLS.TLS10,TLS.TLS11,TLS.TLS12]
            , TLS.pCiphers         = ciphers
            , TLS.pCertificates    = [(cert, Just pk)]
            }
    bracket
        (bindPort (settingsPort set) (settingsHost set))
        sClose
        (\sock -> runSettingsConnection set (getter params sock) app)
  where
    retry :: TLS.TLSParams -> Socket -> SomeException -> IO (Connection, SockAddr)
    retry a b _ = getter a b

    getter params sock = do
        (s, sa) <- accept sock
        handle (retry params sock) $ do
            h <- socketToHandle s ReadWriteMode
            hSetBuffering h NoBuffering
            gen <- newGenIO
            ctx <- TLS.server params (gen :: SystemRandom) h
            TLS.handshake ctx
            let conn = Connection
                    { connSendMany = TLS.sendData ctx . L.fromChunks
                    , connSendAll = TLS.sendData ctx . L.fromChunks . return
                    , connSendFile = \fp offset len _th -> C.runResourceT $ sourceFileRange fp (Just offset) (Just len) C.$$ CL.mapM_ (TLS.sendData ctx . L.fromChunks . return)
                    , connClose = do
                        TLS.bye ctx
                        hClose h
                    , connRecv = TLS.recvData ctx
                    }
            return (conn, sa)

-- taken from stunnel example in tls-extra
ciphers :: [TLS.Cipher]
ciphers =
    [ TLSExtra.cipher_AES128_SHA1
    , TLSExtra.cipher_AES256_SHA1
    , TLSExtra.cipher_RC4_128_MD5
    , TLSExtra.cipher_RC4_128_SHA1
    ]

readCertificate :: FilePath -> IO X509.X509
readCertificate filepath = do
    content <- B.readFile filepath
    certdata <-
        case PEM.parsePEMCert content of
            Nothing -> error "no valid certificate section"
            Just x  -> return x
    case X509.decodeCertificate $ L.fromChunks [certdata] of
        Left err -> error ("cannot decode certificate: " ++ err)
        Right x  -> return x

readPrivateKey :: FilePath -> IO TLS.PrivateKey
readPrivateKey filepath = do
    content <- B.readFile filepath
    pkdata <-
        case PEM.parsePEMKeyRSA content of
            Nothing -> error "no valid RSA key section"
            Just x  -> return (L.fromChunks [x])
    case KeyRSA.decodePrivate pkdata of
        Left err -> error ("cannot decode key: " ++ err)
        Right (_pub, x)  -> return $ TLS.PrivRSA x
