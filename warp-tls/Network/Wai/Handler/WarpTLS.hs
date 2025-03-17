{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

-- | HTTP over TLS support for Warp via the TLS package.
--
--   If HTTP\/2 is negotiated by ALPN, HTTP\/2 over TLS is used.
--   Otherwise HTTP\/1.1 over TLS is used.
--
--   Support for SSL is now obsoleted.
module Network.Wai.Handler.WarpTLS (
    -- * Runner
    runTLS,
    runTLSSocket,

    -- * Settings
    TLSSettings,
    defaultTlsSettings,

    -- * Smart constructors

    -- ** From files
    tlsSettings,
    tlsSettingsChain,

    -- ** From memory
    tlsSettingsMemory,
    tlsSettingsChainMemory,

    -- ** From references
    tlsSettingsRef,
    tlsSettingsChainRef,
    CertSettings,

    -- ** Dynamically retrieved
    tlsSettingsSni,

    -- * Accessors
    tlsCredentials,
    tlsLogging,
    tlsAllowedVersions,
    tlsCiphers,
    tlsWantClientCert,
    tlsServerHooks,
    tlsServerDHEParams,
    tlsSessionManagerConfig,
    tlsSessionManager,
    onInsecure,
    OnInsecure (..),

    -- * Exception
    WarpTLSException (..),

    -- * Low-level
    attachConn
) where

import Control.Applicative ((<|>))
import Control.Exception (
    Exception,
    IOException,
    SomeException (..),
    bracket,
    finally,
    fromException,
    handle,
    handleJust,
    onException,
    throwIO,
    try,
 )
import Control.Monad (guard, void)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.IORef as I
import Data.Streaming.Network (bindPortTCP, safeRecv)
import Data.Typeable (Typeable)
import GHC.IO.Exception (IOErrorType (..))
import Network.Socket (
    SockAddr,
    Socket,
    close,
    getSocketName,
#if MIN_VERSION_network(3,1,1)
    gracefulClose,
#endif
    withSocketsDo,
 )
import qualified Control.Exception as E
import Network.Socket.BufferPool
import Network.Socket.ByteString (sendAll)
import qualified Network.TLS as TLS
import qualified Network.TLS.SessionManager as SM
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal
import Network.Wai.Handler.WarpTLS.Internal
import System.IO.Error (ioeGetErrorType, isEOFError)
import System.Timeout (timeout)

----------------------------------------------------------------

-- | A smart constructor for 'TLSSettings' based on 'defaultTlsSettings'.
tlsSettings
    :: FilePath
    -- ^ Certificate file
    -> FilePath
    -- ^ Key file
    -> TLSSettings
tlsSettings cert key =
    defaultTlsSettings
        { certSettings = CertFromFile cert [] key
        }

-- | A smart constructor for 'TLSSettings' that allows specifying
-- chain certificates based on 'defaultTlsSettings'.
--
-- Since 3.0.3
tlsSettingsChain
    :: FilePath
    -- ^ Certificate file
    -> [FilePath]
    -- ^ Chain certificate files
    -> FilePath
    -- ^ Key file
    -> TLSSettings
tlsSettingsChain cert chainCerts key =
    defaultTlsSettings
        { certSettings = CertFromFile cert chainCerts key
        }

-- | A smart constructor for 'TLSSettings', but uses in-memory representations
-- of the certificate and key based on 'defaultTlsSettings'.
--
-- Since 3.0.1
tlsSettingsMemory
    :: S.ByteString
    -- ^ Certificate bytes
    -> S.ByteString
    -- ^ Key bytes
    -> TLSSettings
tlsSettingsMemory cert key =
    defaultTlsSettings
        { certSettings = CertFromMemory cert [] key
        }

-- | A smart constructor for 'TLSSettings', but uses in-memory representations
-- of the certificate and key based on 'defaultTlsSettings'.
--
-- Since 3.0.3
tlsSettingsChainMemory
    :: S.ByteString
    -- ^ Certificate bytes
    -> [S.ByteString]
    -- ^ Chain certificate bytes
    -> S.ByteString
    -- ^ Key bytes
    -> TLSSettings
tlsSettingsChainMemory cert chainCerts key =
    defaultTlsSettings
        { certSettings = CertFromMemory cert chainCerts key
        }

-- | Smart constructor for TLS settings that obtains its credentials during
-- Server Name Indication. Can be used to return different credentials
-- depending on the hostname but also to retrieve dynamically updated
-- credentials from an IORef. Credentials can be loaded from PEM-encoded chain
-- and key files using 'TLS.credentialLoadX509'.
--
-- @since 3.4.13
tlsSettingsSni :: (Maybe TLS.HostName -> IO TLS.Credentials) -> TLSSettings
tlsSettingsSni onServerNameIndicationHook =
  defaultTlsSettings
    { tlsCredentials = Just (TLS.Credentials [])
    , tlsServerHooks = (tlsServerHooks defaultTlsSettings)
      { TLS.onServerNameIndication =  onServerNameIndicationHook
      }
    }

-- | A smart constructor for 'TLSSettings', but uses references to in-memory
-- representations of the certificate and key based on 'defaultTlsSettings'.
--
-- @since 3.3.0
tlsSettingsRef
    :: I.IORef S.ByteString
    -- ^ Reference to certificate bytes
    -> I.IORef S.ByteString
    -- ^ Reference to key bytes
    -> TLSSettings
tlsSettingsRef cert key =
    defaultTlsSettings
        { certSettings = CertFromRef cert [] key
        }

{-# DEPRECATED tlsSettingsRef "This function was added to allow Warp to serve new certificates without restarting, but it has always behaved the same as 'tlsSettingsMemory'. It will be removed in the next major release. To retain existing behavior, swich to 'tlsSettingsMemory'. To dynamically update credentials, see 'tlsSettingsSni'." #-}

-- | A smart constructor for 'TLSSettings', but uses references to in-memory
-- representations of the certificate and key based on 'defaultTlsSettings'.
--
-- @since 3.3.0
tlsSettingsChainRef
    :: I.IORef S.ByteString
    -- ^ Reference to certificate bytes
    -> [I.IORef S.ByteString]
    -- ^ Reference to chain certificate bytes
    -> I.IORef S.ByteString
    -- ^ Reference to key bytes
    -> TLSSettings
tlsSettingsChainRef cert chainCerts key =
    defaultTlsSettings
        { certSettings = CertFromRef cert chainCerts key
        }

{-# DEPRECATED tlsSettingsChainRef "This function was added to allow Warp to serve new certificates without restarting, but it has always behaved the same as 'tlsSettingsChainMemory'. It will be removed in the next major release. To retain existing behavior, swich to 'tlsSettingsChainMemory'. To dynamically update credentials, see 'tlsSettingsSni'." #-}

----------------------------------------------------------------

-- | Running 'Application' with 'TLSSettings' and 'Settings'.
runTLS :: TLSSettings -> Settings -> Application -> IO ()
runTLS tset set app =
    withSocketsDo $
        bracket
            (bindPortTCP (getPort set) (getHost set))
            close
            ( \sock -> do
                setSocketCloseOnExec sock
                runTLSSocket tset set sock app
            )

----------------------------------------------------------------

loadCredentials :: TLSSettings -> IO TLS.Credentials
loadCredentials TLSSettings{tlsCredentials = Just creds} = return creds
loadCredentials TLSSettings{..} = case certSettings of
    CertFromFile cert chainFiles key -> do
        cred <- either error id <$> TLS.credentialLoadX509Chain cert chainFiles key
        return $ TLS.Credentials [cred]
    CertFromRef certRef chainCertsRef keyRef -> do
        cert <- I.readIORef certRef
        chainCerts <- mapM I.readIORef chainCertsRef
        key <- I.readIORef keyRef
        cred <-
            either error return $ TLS.credentialLoadX509ChainFromMemory cert chainCerts key
        return $ TLS.Credentials [cred]
    CertFromMemory certMemory chainCertsMemory keyMemory -> do
        cred <-
            either error return $
                TLS.credentialLoadX509ChainFromMemory certMemory chainCertsMemory keyMemory
        return $ TLS.Credentials [cred]

getSessionManager :: TLSSettings -> IO TLS.SessionManager
getSessionManager TLSSettings{tlsSessionManager = Just mgr} = return mgr
getSessionManager TLSSettings{..} = case tlsSessionManagerConfig of
    Nothing -> return TLS.noSessionManager
    Just config -> SM.newSessionManager config

-- | Running 'Application' with 'TLSSettings' and 'Settings' using
--   specified 'Socket'.
runTLSSocket :: TLSSettings -> Settings -> Socket -> Application -> IO ()
runTLSSocket tlsset set sock app = do
    settingsInstallShutdownHandler set (close sock)
    credentials <- loadCredentials tlsset
    mgr <- getSessionManager tlsset
    runTLSSocket' tlsset set credentials mgr sock app

runTLSSocket'
    :: TLSSettings
    -> Settings
    -> TLS.Credentials
    -> TLS.SessionManager
    -> Socket
    -> Application
    -> IO ()
runTLSSocket' tlsset@TLSSettings{..} set credentials mgr sock =
    runSettingsConnectionMakerSecure set get
  where
    get = getter tlsset set sock params
    params =
        TLS.defaultParamsServer
            { TLS.serverWantClientCert = tlsWantClientCert
            , TLS.serverCACertificates = []
            , TLS.serverDHEParams = tlsServerDHEParams
            , TLS.serverHooks = hooks
            , TLS.serverShared = shared
            , TLS.serverSupported = supported
#if MIN_VERSION_tls(1,5,0)
            , TLS.serverEarlyDataSize = 2018
#endif
            }
    -- Adding alpn to user's tlsServerHooks.
    hooks =
        tlsServerHooks
            { TLS.onALPNClientSuggest =
                TLS.onALPNClientSuggest tlsServerHooks
                    <|> (if settingsHTTP2Enabled set then Just alpn else Nothing)
            }
    shared =
        TLS.defaultShared
            { TLS.sharedCredentials = credentials
            , TLS.sharedSessionManager = mgr
            }
    supported =
        TLS.defaultSupported
            { TLS.supportedVersions = tlsAllowedVersions
            , TLS.supportedCiphers = tlsCiphers
            , TLS.supportedCompressions = [TLS.nullCompression]
            , TLS.supportedSecureRenegotiation = True
            , TLS.supportedClientInitiatedRenegotiation = False
            , TLS.supportedSession = True
            , TLS.supportedFallbackScsv = True
            , TLS.supportedHashSignatures = tlsSupportedHashSignatures
#if MIN_VERSION_tls(1,5,0)
            , TLS.supportedGroups = [TLS.X25519,TLS.P256,TLS.P384]
#endif
            }

alpn :: [S.ByteString] -> IO S.ByteString
alpn xs
    | "h2" `elem` xs = return "h2"
    | otherwise = return "http/1.1"

----------------------------------------------------------------

getter
    :: TLS.TLSParams params
    => TLSSettings
    -> Settings
    -> Socket
    -> params
    -> IO (IO (Connection, Transport), SockAddr)
getter tlsset set@Settings{settingsAccept = accept'} sock params = do
    (s, sa) <- accept' sock
    setSocketCloseOnExec s
    return (mkConn tlsset set s params, sa)

mkConn
    :: TLS.TLSParams params
    => TLSSettings
    -> Settings
    -> Socket
    -> params
    -> IO (Connection, Transport)
mkConn tlsset set s params = do
    let tm = settingsTimeout set * 1000000
    mbs <- timeout tm recvFirstBS
    case mbs of
      Nothing -> throwIO IncompleteHeaders
      Just bs -> switch bs
  where
    recvFirstBS = safeRecv s 4096 `onException` close s
    switch firstBS
        | S.null firstBS = close s >> throwIO ClientClosedConnectionPrematurely
        | S.head firstBS == 0x16 = httpOverTls tlsset set s firstBS params
        | otherwise = plainHTTP tlsset set s firstBS

----------------------------------------------------------------

isAsyncException :: Exception e => e -> Bool
isAsyncException e =
    case E.fromException (E.toException e) of
        Just (E.SomeAsyncException _) -> True
        Nothing -> False

throughAsync :: IO a -> SomeException -> IO a
throughAsync action (SomeException e)
  | isAsyncException e = E.throwIO e
  | otherwise          = action

httpOverTls
    :: TLS.TLSParams params
    => TLSSettings
    -> Settings
    -> Socket
    -> S.ByteString
    -> params
    -> IO (Connection, Transport)
httpOverTls TLSSettings{..} set s bs0 params =
    makeConn `onException` close s
  where
    makeConn = do
        pool <- newBufferPool 2048 16384
        rawRecvN <- makeRecvN bs0 $ receive s pool
        let recvN = wrappedRecvN rawRecvN
        ctx <- TLS.contextNew (backend recvN) params
        TLS.contextHookSetLogging ctx tlsLogging
        let tm = settingsTimeout set * 1000000
        mconn <- timeout tm $ do
            TLS.handshake ctx
            mysa <- getSocketName s
            attachConn mysa ctx
        case mconn of
          Nothing -> throwIO IncompleteHeaders
          Just conn -> return conn
    wrappedRecvN recvN n = handle (throughAsync (return "")) $ recvN n
    backend recvN =
        TLS.Backend
            { TLS.backendFlush = return ()
#if MIN_VERSION_network(3,1,1)
            , TLS.backendClose =
                gracefulClose s 5000 `E.catch` throughAsync (return ())
#else
            , TLS.backendClose = close s
#endif
            , TLS.backendSend = sendAll' s
            , TLS.backendRecv = recvN
            }
    sendAll' sock bs =
        E.handleJust
            ( \e ->
                if ioeGetErrorType e == ResourceVanished
                    then Just ConnectionClosedByPeer
                    else Nothing
            )
            throwIO
            $ sendAll sock bs

-- | Get "Connection" and "Transport" for a TLS connection that is already did the handshake.
-- @since 3.4.7
attachConn :: SockAddr -> TLS.Context -> IO (Connection, Transport)
attachConn mysa ctx = do
    h2 <- (== Just "h2") <$> TLS.getNegotiatedProtocol ctx
    isH2 <- I.newIORef h2
    writeBuffer <- createWriteBuffer 16384
    writeBufferRef <- I.newIORef writeBuffer
    -- Creating a cache for leftover input data.
    tls <- getTLSinfo ctx
    return (conn writeBufferRef isH2, tls)
  where
    conn writeBufferRef isH2 =
        Connection
            { connSendMany = TLS.sendData ctx . L.fromChunks
            , connSendAll = sendall
            , connSendFile = sendfile
            , connClose = close'
            , connRecv = recv
            , connRecvBuf = \_ _ -> return True -- obsoleted
            , connWriteBuffer = writeBufferRef
            , connHTTP2 = isH2
            , connMySockAddr = mysa
            }
      where
        sendall = TLS.sendData ctx . L.fromChunks . return
        recv = handle onEOF $ TLS.recvData ctx
          where
            onEOF e
#if MIN_VERSION_tls(1,8,0)
                | Just (TLS.PostHandshake TLS.Error_EOF) <- E.fromException e = return S.empty
#else
                | Just TLS.Error_EOF <- fromException e = return S.empty
#endif
                | Just ioe <- fromException e, isEOFError ioe = return S.empty
                | otherwise = throwIO e
        sendfile fid offset len hook headers = do
            writeBuffer <- I.readIORef writeBufferRef
            readSendFile
                (bufBuffer writeBuffer)
                (bufSize writeBuffer)
                sendall
                fid
                offset
                len
                hook
                headers

        close' =
            void (tryIO sendBye)
                `finally` TLS.contextClose ctx

        sendBye =
            -- It's fine if the connection was closed by the other side before
            -- receiving close_notify, see RFC 5246 section 7.2.1.
            handleJust
                (\e -> guard (e == ConnectionClosedByPeer) >> return e)
                (const (return ()))
                (TLS.bye ctx)

getTLSinfo :: TLS.Context -> IO Transport
getTLSinfo ctx = do
    proto <- TLS.getNegotiatedProtocol ctx
    minfo <- TLS.contextGetInformation ctx
    case minfo of
        Nothing -> return TCP
        Just info -> do
            let (major, minor) = case TLS.infoVersion info of
                    TLS.SSL2 -> (2, 0)
                    TLS.SSL3 -> (3, 0)
                    TLS.TLS10 -> (3, 1)
                    TLS.TLS11 -> (3, 2)
                    TLS.TLS12 -> (3, 3)
                    _ -> (3,4)
            clientCert <- TLS.getClientCertificateChain ctx
            return
                TLS
                    { tlsMajorVersion = major
                    , tlsMinorVersion = minor
                    , tlsNegotiatedProtocol = proto
                    , tlsChiperID = TLS.cipherID $ TLS.infoCipher info
                    , tlsClientCertificate = clientCert
                    }

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

----------------------------------------------------------------

plainHTTP
    :: TLSSettings -> Settings -> Socket -> S.ByteString -> IO (Connection, Transport)
plainHTTP TLSSettings{..} set s bs0 = case onInsecure of
    AllowInsecure -> do
        conn' <- socketConnection set s
        cachedRef <- I.newIORef bs0
        let conn'' =
                conn'
                    { connRecv = recvPlain cachedRef (connRecv conn')
                    }
        return (conn'', TCP)
    DenyInsecure lbs -> do
        -- Listening port 443 but TLS records do not arrive.
        -- We want to let the browser know that TLS is required.
        -- So, we use 426.
        --     http://tools.ietf.org/html/rfc2817#section-4.2
        --     https://tools.ietf.org/html/rfc7231#section-6.5.15
        -- FIXME: should we distinguish HTTP/1.1 and HTTP/2?
        --        In the case of HTTP/2, should we send
        --        GOAWAY + INADEQUATE_SECURITY?
        -- FIXME: Content-Length:
        -- FIXME: TLS/<version>
        sendAll
            s
            "HTTP/1.1 426 Upgrade Required\
            \\r\nUpgrade: TLS/1.0, HTTP/1.1\
            \\r\nConnection: Upgrade\
            \\r\nContent-Type: text/plain\r\n\r\n"
        mapM_ (sendAll s) $ L.toChunks lbs
        close s
        throwIO InsecureConnectionDenied

----------------------------------------------------------------

-- | Modify the given receive function to first check the given @IORef@ for a
-- chunk of data. If present, takes the chunk of data from the @IORef@ and
-- empties out the @IORef@. Otherwise, calls the supplied receive function.
recvPlain :: I.IORef S.ByteString -> IO S.ByteString -> IO S.ByteString
recvPlain ref fallback = do
    bs <- I.readIORef ref
    if S.null bs
        then fallback
        else do
            I.writeIORef ref S.empty
            return bs

----------------------------------------------------------------

data WarpTLSException
    = InsecureConnectionDenied
    | ClientClosedConnectionPrematurely
    deriving (Show, Typeable)
instance Exception WarpTLSException
