{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | HTTP over SSL/TLS support for Warp via the TLS package.

module Network.Wai.Handler.WarpTLS (
    -- * Settings
      TLSSettings
    , certFile
    , keyFile
    , onInsecure
    , tlsLogging
    , defaultTlsSettings
    , tlsSettings
    , OnInsecure (..)
    -- * Runner
    , runTLS
    , runTLSSocket
    -- * Exception
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
import Control.Exception (bracket, finally, handle)
import qualified Network.TLS.Extra as TLSExtra
import qualified Data.ByteString as B
import Data.Conduit.Network (bindPort)
import Control.Applicative ((<$>))
import Data.Conduit.Network (sourceSocket, sinkSocket, acceptSafe)
import Data.Maybe (fromMaybe)
import qualified Data.IORef as I
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import Data.Default.Class
import qualified Data.Conduit.Binary as CB
#if MIN_VERSION_tls(1, 1, 3)
import qualified Crypto.Random.AESCtr
#endif
import Network.Wai.Handler.Warp.Buffer

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
    , tlsAllowedVersions :: [TLS.Version]
      -- ^ The TLS versions this server accepts.
      --
      -- Default: '[TLS.SSL3,TLS.TLS10,TLS.TLS11,TLS.TLS12]'.
      --
      -- Since 1.4.2
    , tlsCiphers :: [TLS.Cipher]
      -- ^ The TLS ciphers this server accepts.
      --
      -- Default: '[TLSExtra.cipher_AES128_SHA1, TLSExtra.cipher_AES256_SHA1, TLSExtra.cipher_RC4_128_MD5, TLSExtra.cipher_RC4_128_SHA1]'
      --
      -- Since 1.4.2
    }

-- | An action when a plain HTTP comes to HTTP over TLS/SSL port.
data OnInsecure = DenyInsecure L.ByteString
                | AllowInsecure

-- | A smart constructor for 'TLSSettings'.
tlsSettings :: FilePath -- ^ Certificate file
            -> FilePath -- ^ Key file
            -> TLSSettings
tlsSettings cert key = defaultTlsSettings
    { certFile = cert
    , keyFile = key
    }

-- | Default 'TLSSettings'. Use this to create 'TLSSettings' with the field record name.
defaultTlsSettings :: TLSSettings
defaultTlsSettings = TLSSettings
    { certFile = "certificate.pem"
    , keyFile = "key.pem"
    , onInsecure = DenyInsecure "This server only accepts secure HTTPS connections."
    , tlsLogging = def
    , tlsAllowedVersions = [TLS.SSL3,TLS.TLS10,TLS.TLS11,TLS.TLS12]
    , tlsCiphers = ciphers
    }

-- | Running 'Application' with 'TLSSettings' and 'Settings' using
--   specified 'Socket'.
runTLSSocket :: TLSSettings -> Settings -> Socket -> Application -> IO ()
runTLSSocket TLSSettings {..} set sock app = do
    credential <- either error id <$> TLS.credentialLoadX509 certFile keyFile
    let params = def
            { TLS.serverWantClientCert = False
            , TLS.serverSupported = def
                { TLS.supportedVersions = tlsAllowedVersions
                , TLS.supportedCiphers  = tlsCiphers
                }
            , TLS.serverShared = def
                { TLS.sharedCredentials = TLS.Credentials [credential]
                }
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
#if MIN_VERSION_tls(1, 1, 3)
                    gen <- Crypto.Random.AESCtr.makeSystem
#else
                    gen <- makeSystem
#endif
                    ctx <- TLS.contextNew
                        TLS.Backend
                            { TLS.backendFlush = return ()
                            , TLS.backendClose = sClose s
                            , TLS.backendSend = \bs -> C.yield bs C.$$ toClient
                            , TLS.backendRecv = getNext . fmap (B.concat . L.toChunks) . CB.take
                            }
                        params
                        gen
                    TLS.contextHookSetLogging ctx tlsLogging
                    TLS.handshake ctx
                    buf <- allocateBuffer bufferSize
                    let conn = Connection
                            { connSendMany = TLS.sendData ctx . L.fromChunks
                            , connSendAll = TLS.sendData ctx . L.fromChunks . return
                            , connSendFile = \fp offset len _th headers -> do
                                TLS.sendData ctx $ L.fromChunks headers
                                C.runResourceT $ sourceFileRange fp (Just offset) (Just len) C.$$ CL.mapM_ (TLS.sendData ctx . L.fromChunks . return)
                            , connClose =
                                TLS.bye ctx `finally`
                                TLS.contextClose ctx
                            , connRecv =
                                let onEOF TLS.Error_EOF = return B.empty
                                    onEOF e = throwIO e
                                    go = handle onEOF $ do
                                        x <- TLS.recvData ctx
                                        if B.null x
                                            then go
                                            else return x
                                 in go
                            , connSendFileOverride = NotOverride
                            , connBuffer = buf
                            , connBufferSize = bufferSize
                            }
                    return conn
                else
                    case onInsecure of
                        AllowInsecure -> do
                            conn' <- socketConnection s
                            return conn'
                                    { connRecv = getNext $ fmap (fromMaybe B.empty) C.await
                                    }
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

-- | Running 'Application' with 'TLSSettings' and 'Settings'.
runTLS :: TLSSettings -> Settings -> Application -> IO ()
runTLS tset set app = withSocketsDo $
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
