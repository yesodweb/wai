{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | HTTP over SSL/TLS support for Warp via the TLS package

module Network.Wai.Handler.WarpTLS
    ( TLSSettings
    , certFile
    , keyFile
    , onInsecure
    , tlsLogging
    , OnInsecure (..)
    , tlsSettings
    , runTLS
    , runTLSSocket
    , WarpTLSException (..)
    ) where

import qualified Network.TLS as TLS
import Network.Wai.Handler.Warp
import Network.Wai
import Network.Socket
import qualified Data.ByteString.Lazy as L
import Data.Conduit.Binary (sourceFileRange)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Exception (bracket)
import qualified Network.TLS.Extra as TLSExtra
import qualified Data.Certificate.X509 as X509
import qualified Data.ByteString as B
import qualified Data.Certificate.KeyRSA as KeyRSA
import Data.Conduit.Network (bindPort)
import Data.Either (rights)
import Control.Applicative ((<$>))
import qualified Data.PEM as PEM
import Data.Conduit.Network (sourceSocket, sinkSocket, acceptSafe)
import Data.Maybe (fromMaybe)
import qualified Data.IORef as I
import Control.Monad (unless)
import Crypto.Random.API (getSystemRandomGen)
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import qualified Data.Conduit.Binary as CB

data TLSSettings = TLSSettings
    { certFile :: FilePath
      -- ^ File containing the certificate.
    , keyFile :: FilePath
      -- ^ File containing the key
    , onInsecure :: OnInsecure
      -- ^ Do we allow insecure connections with this server as well? Default
      -- is a simple text response stating that a secure connection is required.
      --
      -- Since 1.4.0
    , tlsLogging :: TLS.Logging
      -- ^ The level of logging to turn on.
      --
      -- Default: 'TLS.defaultLogging'.
      --
      -- Since 1.4.0
    }

data OnInsecure = DenyInsecure L.ByteString
                | AllowInsecure

tlsSettings :: FilePath -- ^ Certificate file
            -> FilePath -- ^ key file
            -> TLSSettings
tlsSettings cert key = TLSSettings
    { certFile = cert
    , keyFile = key
    , onInsecure = DenyInsecure "This server only accepts secure HTTPS connections."
    , tlsLogging = TLS.defaultLogging
    }

runTLSSocket :: TLSSettings -> Settings -> Socket -> Application -> IO ()
runTLSSocket TLSSettings {..} set sock app = do
    certs   <- readCertificates certFile
    pk      <- readPrivateKey keyFile
    let params =
            TLS.updateServerParams
                (\sp -> sp { TLS.serverWantClientCert = False }) $
            TLS.defaultParamsServer
            { TLS.pAllowedVersions = [TLS.SSL3,TLS.TLS10,TLS.TLS11,TLS.TLS12]
            , TLS.pCiphers         = ciphers
            , TLS.pCertificates    = zip certs $ (Just pk):repeat Nothing
            , TLS.pLogging         = tlsLogging
            }
    runSettingsConnectionMaker set (getter params) app
  where
    getter params = do
        (s, sa) <- acceptSafe sock
        let mkConn = do
            (fromClient, firstBS) <- sourceSocket s C.$$+ CL.peek
            let toClient = sinkSocket s
            ifromClient <- I.newIORef fromClient
            let getNext sink = do
                    fromClient' <- I.readIORef ifromClient
                    (fromClient'', bs) <- fromClient' C.$$++ sink
                    I.writeIORef ifromClient fromClient''
                    return bs
            if maybe False ((== 0x16) . fst) (firstBS >>= B.uncons)
                then do
                    gen <- getSystemRandomGen
                    ctx <- TLS.contextNew
                        TLS.Backend
                            { TLS.backendFlush = return ()
                            , TLS.backendClose = return ()
                            , TLS.backendSend = \bs -> C.yield bs C.$$ toClient
                            , TLS.backendRecv = getNext . fmap (B.concat . L.toChunks) . CB.take
                            }
                        params
                        gen
                    TLS.handshake ctx
                    let conn = Connection
                            { connSendMany = TLS.sendData ctx . L.fromChunks
                            , connSendAll = TLS.sendData ctx . L.fromChunks . return
                            , connSendFile = \fp offset len _th headers _cleaner -> do
                                TLS.sendData ctx $ L.fromChunks headers
                                C.runResourceT $ sourceFileRange fp (Just offset) (Just len) C.$$ CL.mapM_ (TLS.sendData ctx . L.fromChunks . return)
                            , connClose = do
                                TLS.bye ctx
                                sClose s
                            , connRecv = TLS.recvData ctx
                            }
                    return conn
                else
                    case onInsecure of
                        AllowInsecure ->
                            let conn = (socketConnection s)
                                    { connRecv = getNext $ fmap (fromMaybe B.empty) C.await
                                    }
                             in return conn
                        DenyInsecure lbs -> do
                            let src = do
                                    C.yield "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\n"
                                    mapM_ C.yield $ L.toChunks lbs
                            src C.$$ sinkSocket s
                            sClose s
                            throwIO InsecureConnectionDenied
        return (mkConn, sa)

data WarpTLSException = InsecureConnectionDenied
    deriving (Show, Typeable)
instance Exception WarpTLSException

runTLS :: TLSSettings -> Settings -> Application -> IO ()
runTLS tset set app =
    bracket
        (bindPort (settingsPort set) (settingsHost set))
        sClose
        (\sock -> runTLSSocket tset set sock app)

-- taken from stunnel example in tls-extra
ciphers :: [TLS.Cipher]
ciphers =
    [ TLSExtra.cipher_AES128_SHA1
    , TLSExtra.cipher_AES256_SHA1
    , TLSExtra.cipher_RC4_128_MD5
    , TLSExtra.cipher_RC4_128_SHA1
    ]

readCertificates :: FilePath -> IO [X509.X509]
readCertificates filepath = do
    certs <- rights . parseCerts . PEM.pemParseBS <$> B.readFile filepath
    case certs of
        []-> error "no valid certificate found"
        x -> return x
    where parseCerts (Right pems) = map (X509.decodeCertificate . L.fromChunks . (:[]) . PEM.pemContent)
                                  $ filter (flip elem ["CERTIFICATE", "TRUSTED CERTIFICATE"] . PEM.pemName) pems
          parseCerts (Left err) = error $ "cannot parse PEM file: " ++ err

readPrivateKey :: FilePath -> IO TLS.PrivateKey
readPrivateKey filepath = do
    pk <- rights . parseKey . PEM.pemParseBS <$> B.readFile filepath
    case pk of
        []    -> error "no valid RSA key found"
        (x:_) -> return x

    where parseKey (Right pems) = map (fmap (TLS.PrivRSA . snd) . KeyRSA.decodePrivate . L.fromChunks . (:[]) . PEM.pemContent)
                                $ filter ((== "RSA PRIVATE KEY") . PEM.pemName) pems
          parseKey (Left err) = error $ "Cannot parse PEM file: " ++ err
