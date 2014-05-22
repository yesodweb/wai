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
    , tlsAllowedVersions
    , tlsCiphers
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
import Network.Wai (Application)
import Network.Socket (Socket, sClose, withSocketsDo)
import qualified Data.ByteString.Lazy as L
import Control.Exception (bracket, finally, handle)
import qualified Network.TLS.Extra as TLSExtra
import qualified Data.ByteString as B
import Data.Streaming.Network (bindPortTCP, acceptSafe, safeRecv)
import Control.Applicative ((<$>))
import qualified Data.IORef as I
import Control.Exception (Exception, throwIO)
import Data.Typeable (Typeable)
import Data.Default.Class (def)
import qualified Crypto.Random.AESCtr
import Network.Wai.Handler.Warp.Buffer (allocateBuffer, bufferSize)
import Network.Socket.ByteString (sendAll)
import Control.Monad (unless)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import qualified System.IO as IO

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
    runSettingsConnectionMakerSecure set (getter params) app
  where
    getter params = do
        (s, sa) <- acceptSafe sock
        let mkConn :: IO (Connection, Bool)
            mkConn = do
            firstBS <- safeRecv s 4096
            cachedRef <- I.newIORef firstBS
            let getNext size = do
                    cached <- I.readIORef cachedRef
                    loop cached
                  where
                    loop bs | B.length bs >= size = do
                        let (x, y) = B.splitAt size bs
                        I.writeIORef cachedRef y
                        return x
                    loop bs1 = do
                        bs2 <- safeRecv s 4096
                        if B.null bs2
                            then do
                                -- FIXME does this deserve an exception being thrown?
                                I.writeIORef cachedRef B.empty
                                return bs1
                            else loop $ B.append bs1 bs2
            if not (B.null firstBS) && B.head firstBS == 0x16
                then do
                    gen <- Crypto.Random.AESCtr.makeSystem
                    ctx <- TLS.contextNew
                        TLS.Backend
                            { TLS.backendFlush = return ()
                            , TLS.backendClose = sClose s
                            , TLS.backendSend = sendAll s
                            , TLS.backendRecv = getNext
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
                                IO.withBinaryFile fp IO.ReadMode $ \h -> do
                                    IO.hSeek h IO.AbsoluteSeek offset
                                    let loop remaining | remaining <= 0 = return ()
                                        loop remaining = do
                                            bs <- B.hGetSome h defaultChunkSize
                                            unless (B.null bs) $ do
                                                let x = B.take remaining bs
                                                TLS.sendData ctx $ L.fromChunks [x]
                                                loop $ remaining - B.length x
                                    loop $ fromIntegral len
                            , connClose =
                                TLS.bye ctx `finally`
                                TLS.contextClose ctx
                            , connRecv =
                                let onEOF TLS.Error_EOF = return B.empty
                                    onEOF e = throwIO e
                                    go = do
                                        x <- TLS.recvData ctx
                                        if B.null x
                                            then go
                                            else return x
                                 in handle onEOF go
                            , connSendFileOverride = NotOverride
                            , connBuffer = buf
                            , connBufferSize = bufferSize
                            }
                    return (conn, True)
                else
                    case onInsecure of
                        AllowInsecure -> do
                            conn' <- socketConnection s
                            return (conn'
                                    { connRecv = getNext 4096
                                    }, False)
                        DenyInsecure lbs -> do
                            sendAll s "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\n\r\n"
                            mapM_ (sendAll s) $ L.toChunks lbs
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
        (bindPortTCP (getPort set) (getHost set))
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
