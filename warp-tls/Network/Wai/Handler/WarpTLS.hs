{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WarpTLS
    ( TLSSettings (..)
    , runTLS
    ) where

import qualified Network.TLS as TLS
import Network.Wai.Handler.Warp
import Network.Wai
import Network.Socket
import Crypto.Random
import qualified Data.ByteString.Lazy as L
import Data.Conduit.Binary (sourceFileRange)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Exception (bracket, handle, SomeException)
import qualified Network.TLS.Extra as TLSExtra
import qualified Data.Certificate.X509 as X509
import qualified Data.ByteString as B
import qualified Data.Certificate.KeyRSA as KeyRSA
import Data.Conduit.Network (bindPort)
import Data.Either (rights)
import Control.Applicative ((<$>))
import qualified Data.PEM as PEM
import Data.Conduit.Network (sourceSocket, sinkSocket)
import Data.Maybe (fromMaybe)
import qualified Data.IORef as I
import Control.Monad (unless)

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
    retry :: Socket -> TLS.TLSParams -> Socket -> SomeException -> IO (Connection, SockAddr)
    retry s a b _ = sClose s >> getter a b

    getter params sock = do
        (s, sa) <- accept sock
        handle (retry s params sock) $ do
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
                    gen <- newGenIO
                    ctx <- TLS.serverWith
                        params
                        (gen :: SystemRandom)
                        s
                        (return ()) -- flush
                        (\bs -> C.yield bs C.$$ toClient)
                        (getNext . takeMost)
                    TLS.handshake ctx
                    let conn = Connection
                            { connSendMany = TLS.sendData ctx . L.fromChunks
                            , connSendAll = TLS.sendData ctx . L.fromChunks . return
                            , connSendFile = \fp offset len _th headers -> do
                                TLS.sendData ctx $ L.fromChunks headers
                                C.runResourceT $ sourceFileRange fp (Just offset) (Just len) C.$$ CL.mapM_ (TLS.sendData ctx . L.fromChunks . return)
                            , connClose = do
                                TLS.bye ctx
                                sClose s
                            , connRecv = TLS.recvData ctx
                            }
                    return (conn, sa)
                else do
                    let conn = (socketConnection s)
                            { connRecv = getNext $ fmap (fromMaybe B.empty) C.await
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
    certs <- rights . parseCerts . PEM.pemParseBS <$> B.readFile filepath
    case certs of
        []    -> error "no valid certificate found"
        (x:_) -> return x
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

takeMost :: Monad m => Int -> C.GLSink B.ByteString m B.ByteString
takeMost i =
    C.await >>= maybe (return B.empty) go
  where
    go bs = do
        unless (B.null y) $ C.leftover y
        return x
      where
        (x, y) = B.splitAt i bs
