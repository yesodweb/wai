{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternGuards #-}

-- | HTTP over TLS support for Warp via the TLS package.
--
--   If HTTP\/2 is negotiated by ALPN, HTTP\/2 over TLS is used.
--   Otherwise HTTP\/1.1 over TLS is used.
--
--   Support for SSL is now obsoleted.

module Network.Wai.Handler.WarpTLS (
    -- * Runner
      runTLS
    , runTLSSocket
    -- * Settings
    , TLSSettings
    , defaultTlsSettings
    -- * Smart constructors
    -- ** From files
    , tlsSettings
    , tlsSettingsChain
    -- ** From memory
    , tlsSettingsMemory
    , tlsSettingsChainMemory
    -- ** From references
    , tlsSettingsRef
    , tlsSettingsChainRef
    , CertSettings
    -- * Accessors
    , tlsCredentials
    , tlsLogging
    , tlsAllowedVersions
    , tlsCiphers
    , tlsWantClientCert
    , tlsServerHooks
    , tlsServerDHEParams
    , tlsSessionManagerConfig
    , tlsSessionManager
    , onInsecure
    , OnInsecure (..)
    -- * Exception
    , WarpTLSException (..)
    -- * DH parameters (re-exports)
    --
    -- | This custom DH parameters are not necessary anymore because
    --   pre-defined DH parameters are supported in the TLS package.
    , DH.Params
    , DH.generateParams
    ) where

import Control.Applicative ((<|>))
import UnliftIO.Exception (Exception, throwIO, bracket, finally, handle, fromException, try, IOException, onException, SomeException(..), handleJust)
import qualified UnliftIO.Exception as E
import Control.Monad (void, guard)
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.Default.Class (def)
import qualified Data.IORef as I
import Data.Streaming.Network (bindPortTCP, safeRecv)
import Data.Typeable (Typeable)
import GHC.IO.Exception (IOErrorType(..))
import Network.Socket (Socket, close, withSocketsDo, SockAddr, accept)
#if MIN_VERSION_network(3,1,1)
import Network.Socket (gracefulClose)
#endif
import Network.Socket.ByteString (sendAll)
import qualified Network.TLS as TLS
import qualified Crypto.PubKey.DH as DH
import qualified Network.TLS.Extra as TLSExtra
import qualified Network.TLS.SessionManager as SM
import Network.Wai (Application)
import Network.Wai.Handler.Warp
import Network.Wai.Handler.Warp.Internal
import Network.Wai.Handler.WarpTLS.Internal(CertSettings(..), TLSSettings(..), OnInsecure(..))
import System.IO.Error (isEOFError, ioeGetErrorType)

-- | The default 'CertSettings'.
defaultCertSettings :: CertSettings
defaultCertSettings = CertFromFile "certificate.pem" [] "key.pem"

----------------------------------------------------------------

-- | Default 'TLSSettings'. Use this to create 'TLSSettings' with the field record name (aka accessors).
defaultTlsSettings :: TLSSettings
defaultTlsSettings = TLSSettings {
    certSettings = defaultCertSettings
  , onInsecure = DenyInsecure "This server only accepts secure HTTPS connections."
  , tlsLogging = def
#if MIN_VERSION_tls(1,5,0)
  , tlsAllowedVersions = [TLS.TLS13,TLS.TLS12,TLS.TLS11,TLS.TLS10]
#else
  , tlsAllowedVersions = [TLS.TLS12,TLS.TLS11,TLS.TLS10]
#endif
  , tlsCiphers = ciphers
  , tlsWantClientCert = False
  , tlsServerHooks = def
  , tlsServerDHEParams = Nothing
  , tlsSessionManagerConfig = Nothing
  , tlsCredentials = Nothing
  , tlsSessionManager = Nothing
  , tlsSupportedHashSignatures = TLS.supportedHashSignatures def
  }

-- taken from stunnel example in tls-extra
ciphers :: [TLS.Cipher]
ciphers = TLSExtra.ciphersuite_strong

----------------------------------------------------------------

-- | A smart constructor for 'TLSSettings' based on 'defaultTlsSettings'.
tlsSettings :: FilePath -- ^ Certificate file
            -> FilePath -- ^ Key file
            -> TLSSettings
tlsSettings cert key = defaultTlsSettings {
    certSettings = CertFromFile cert [] key
  }

-- | A smart constructor for 'TLSSettings' that allows specifying
-- chain certificates based on 'defaultTlsSettings'.
--
-- Since 3.0.3
tlsSettingsChain
            :: FilePath -- ^ Certificate file
            -> [FilePath] -- ^ Chain certificate files
            -> FilePath -- ^ Key file
            -> TLSSettings
tlsSettingsChain cert chainCerts key = defaultTlsSettings {
    certSettings = CertFromFile cert chainCerts key
  }

-- | A smart constructor for 'TLSSettings', but uses in-memory representations
-- of the certificate and key based on 'defaultTlsSettings'.
--
-- Since 3.0.1
tlsSettingsMemory
    :: S.ByteString -- ^ Certificate bytes
    -> S.ByteString -- ^ Key bytes
    -> TLSSettings
tlsSettingsMemory cert key = defaultTlsSettings {
    certSettings = CertFromMemory cert [] key
  }

-- | A smart constructor for 'TLSSettings', but uses in-memory representations
-- of the certificate and key based on 'defaultTlsSettings'.
--
-- Since 3.0.3
tlsSettingsChainMemory
    :: S.ByteString -- ^ Certificate bytes
    -> [S.ByteString] -- ^ Chain certificate bytes
    -> S.ByteString -- ^ Key bytes
    -> TLSSettings
tlsSettingsChainMemory cert chainCerts key = defaultTlsSettings {
    certSettings = CertFromMemory cert chainCerts key
  }

-- | A smart constructor for 'TLSSettings', but uses references to in-memory
-- representations of the certificate and key based on 'defaultTlsSettings'.
--
-- @since 3.3.0
tlsSettingsRef
    :: I.IORef S.ByteString -- ^ Reference to certificate bytes
    -> I.IORef (S.ByteString) -- ^ Reference to key bytes
    -> TLSSettings
tlsSettingsRef cert key = defaultTlsSettings {
    certSettings = CertFromRef cert [] key
  }

-- | A smart constructor for 'TLSSettings', but uses references to in-memory
-- representations of the certificate and key based on 'defaultTlsSettings'.
--
-- @since 3.3.0
tlsSettingsChainRef
    :: I.IORef S.ByteString -- ^ Reference to certificate bytes
    -> [I.IORef S.ByteString] -- ^ Reference to chain certificate bytes
    -> I.IORef (S.ByteString) -- ^ Reference to key bytes
    -> TLSSettings
tlsSettingsChainRef cert chainCerts key = defaultTlsSettings {
    certSettings = CertFromRef cert chainCerts key
  }

----------------------------------------------------------------

-- | Running 'Application' with 'TLSSettings' and 'Settings'.
runTLS :: TLSSettings -> Settings -> Application -> IO ()
runTLS tset set app = withSocketsDo $
    bracket
        (bindPortTCP (getPort set) (getHost set))
        close
        (\sock -> runTLSSocket tset set sock app)

----------------------------------------------------------------

loadCredentials :: TLSSettings -> IO TLS.Credentials
loadCredentials TLSSettings{ tlsCredentials = Just creds } = return creds
loadCredentials TLSSettings{..} = case certSettings of
  CertFromFile cert chainFiles key -> do
    cred <- either error id <$> TLS.credentialLoadX509Chain cert chainFiles key
    return $ TLS.Credentials [cred]
  CertFromRef certRef chainCertsRef keyRef -> do
    cert <- I.readIORef certRef
    chainCerts <- mapM I.readIORef chainCertsRef
    key <- I.readIORef keyRef
    cred <- either error return $ TLS.credentialLoadX509ChainFromMemory cert chainCerts key
    return $ TLS.Credentials [cred]
  CertFromMemory certMemory chainCertsMemory keyMemory -> do
    cred <- either error return $ TLS.credentialLoadX509ChainFromMemory certMemory chainCertsMemory keyMemory
    return $ TLS.Credentials [cred]

getSessionManager :: TLSSettings -> IO TLS.SessionManager
getSessionManager TLSSettings{ tlsSessionManager = Just mgr } = return mgr
getSessionManager TLSSettings{..} = case tlsSessionManagerConfig of
      Nothing     -> return TLS.noSessionManager
      Just config -> SM.newSessionManager config

-- | Running 'Application' with 'TLSSettings' and 'Settings' using
--   specified 'Socket'.
runTLSSocket :: TLSSettings -> Settings -> Socket -> Application -> IO ()
runTLSSocket tlsset set sock app = do
    credentials <- loadCredentials tlsset
    mgr <- getSessionManager tlsset
    runTLSSocket' tlsset set credentials mgr sock app

runTLSSocket' :: TLSSettings -> Settings -> TLS.Credentials -> TLS.SessionManager -> Socket -> Application -> IO ()
runTLSSocket' tlsset@TLSSettings{..} set credentials mgr sock =
    runSettingsConnectionMakerSecure set get
  where
    get = getter tlsset set sock params
    params = def { -- TLS.ServerParams
        TLS.serverWantClientCert = tlsWantClientCert
      , TLS.serverCACertificates = []
      , TLS.serverDHEParams      = tlsServerDHEParams
      , TLS.serverHooks          = hooks
      , TLS.serverShared         = shared
      , TLS.serverSupported      = supported
#if MIN_VERSION_tls(1,5,0)
      , TLS.serverEarlyDataSize  = 2018
#endif
      }
    -- Adding alpn to user's tlsServerHooks.
    hooks = tlsServerHooks {
        TLS.onALPNClientSuggest = TLS.onALPNClientSuggest tlsServerHooks <|>
          (if settingsHTTP2Enabled set then Just alpn else Nothing)
      }
    shared = def {
        TLS.sharedCredentials    = credentials
      , TLS.sharedSessionManager = mgr
      }
    supported = def { -- TLS.Supported
        TLS.supportedVersions       = tlsAllowedVersions
      , TLS.supportedCiphers        = tlsCiphers
      , TLS.supportedCompressions   = [TLS.nullCompression]
      , TLS.supportedSecureRenegotiation = True
      , TLS.supportedClientInitiatedRenegotiation = False
      , TLS.supportedSession             = True
      , TLS.supportedFallbackScsv        = True
      , TLS.supportedHashSignatures      = tlsSupportedHashSignatures
#if MIN_VERSION_tls(1,5,0)
      , TLS.supportedGroups              = [TLS.X25519,TLS.P256,TLS.P384]
#endif
      }

alpn :: [S.ByteString] -> IO S.ByteString
alpn xs
  | "h2"    `elem` xs = return "h2"
  | otherwise         = return "http/1.1"

----------------------------------------------------------------

getter :: TLS.TLSParams params => TLSSettings -> Settings -> Socket -> params -> IO (IO (Connection, Transport), SockAddr)
getter tlsset set sock params = do
#if WINDOWS
    (s, sa) <- windowsThreadBlockHack $ accept sock
#else
    (s, sa) <- accept sock
#endif
    setSocketCloseOnExec s
    return (mkConn tlsset set s params, sa)

mkConn :: TLS.TLSParams params => TLSSettings -> Settings -> Socket -> params -> IO (Connection, Transport)
mkConn tlsset set s params = (safeRecv s 4096 >>= switch) `onException` close s
  where
    switch firstBS
        | S.null firstBS = close s >> throwIO ClientClosedConnectionPrematurely
        | S.head firstBS == 0x16 = httpOverTls tlsset set s firstBS params
        | otherwise = plainHTTP tlsset set s firstBS

----------------------------------------------------------------

httpOverTls :: TLS.TLSParams params => TLSSettings -> Settings -> Socket -> S.ByteString -> params -> IO (Connection, Transport)
httpOverTls TLSSettings{..} _set s bs0 params = do
    recvN <- makePlainReceiveN s bs0
    ctx <- TLS.contextNew (backend recvN) params
    TLS.contextHookSetLogging ctx tlsLogging
    TLS.handshake ctx
    h2 <- (== Just "h2") <$> TLS.getNegotiatedProtocol ctx
    isH2 <- I.newIORef h2
    writeBuf <- allocateBuffer bufferSize
    -- Creating a cache for leftover input data.
    ref <- I.newIORef ""
    tls <- getTLSinfo ctx
    return (conn ctx writeBuf ref isH2, tls)
  where
    backend recvN = TLS.Backend {
        TLS.backendFlush = return ()
#if MIN_VERSION_network(3,1,1)
      , TLS.backendClose = gracefulClose s 5000 `E.catch` \(SomeException _) -> return ()
#else
      , TLS.backendClose = close s
#endif
      , TLS.backendSend  = sendAll' s
      , TLS.backendRecv  = recvN
      }
    sendAll' sock bs = E.handleJust
      (\ e -> if ioeGetErrorType e == ResourceVanished
        then Just ConnectionClosedByPeer
        else Nothing)
      throwIO
      $ sendAll sock bs
    conn ctx writeBuf ref isH2 = Connection {
        connSendMany         = TLS.sendData ctx . L.fromChunks
      , connSendAll          = sendall
      , connSendFile         = sendfile
      , connClose            = close'
      , connFree             = freeBuffer writeBuf
      , connRecv             = recv ref
      , connRecvBuf          = recvBuf ref
      , connWriteBuffer      = writeBuf
      , connBufferSize       = bufferSize
      , connHTTP2            = isH2
      }
      where
        sendall = TLS.sendData ctx . L.fromChunks . return
        sendfile fid offset len hook headers =
            readSendFile writeBuf bufferSize sendall fid offset len hook headers

        close' = void (tryIO sendBye) `finally`
                 TLS.contextClose ctx

        sendBye =
          -- It's fine if the connection was closed by the other side before
          -- receiving close_notify, see RFC 5246 section 7.2.1.
          handleJust
            (\e -> guard (e == ConnectionClosedByPeer) >> return e)
            (const (return ()))
            (TLS.bye ctx)

        -- TLS version of recv with a cache for leftover input data.
        -- The cache is shared with recvBuf.
        recv cref = do
            cached <- I.readIORef cref
            if cached /= "" then do
                I.writeIORef cref ""
                return cached
              else
                recv'

        -- TLS version of recv (decrypting) without a cache.
        recv' = handle onEOF go
          where
            onEOF e
              | Just TLS.Error_EOF <- fromException e       = return S.empty
              | Just ioe <- fromException e, isEOFError ioe = return S.empty                  | otherwise                                   = throwIO e
            go = do
                x <- TLS.recvData ctx
                if S.null x then
                    go
                  else
                    return x

        -- TLS version of recvBuf with a cache for leftover input data.
        recvBuf cref buf siz = do
            cached <- I.readIORef cref
            (ret, leftover) <- fill cached buf siz recv'
            I.writeIORef cref leftover
            return ret

fill :: S.ByteString -> Buffer -> BufSize -> Recv -> IO (Bool,S.ByteString)
fill bs0 buf0 siz0 recv
  | siz0 <= len0 = do
      let (bs, leftover) = S.splitAt siz0 bs0
      void $ copy buf0 bs
      return (True, leftover)
  | otherwise = do
      buf <- copy buf0 bs0
      loop buf (siz0 - len0)
  where
    len0 = S.length bs0
    loop _   0   = return (True, "")
    loop buf siz = do
      bs <- recv
      let len = S.length bs
      if len == 0 then return (False, "")
        else if len <= siz then do
          buf' <- copy buf bs
          loop buf' (siz - len)
        else do
          let (bs1,bs2) = S.splitAt siz bs
          void $ copy buf bs1
          return (True, bs2)

getTLSinfo :: TLS.Context -> IO Transport
getTLSinfo ctx = do
    proto <- TLS.getNegotiatedProtocol ctx
    minfo <- TLS.contextGetInformation ctx
    case minfo of
        Nothing   -> return TCP
        Just TLS.Information{..} -> do
            let (major, minor) = case infoVersion of
                    TLS.SSL2  -> (2,0)
                    TLS.SSL3  -> (3,0)
                    TLS.TLS10 -> (3,1)
                    TLS.TLS11 -> (3,2)
                    TLS.TLS12 -> (3,3)
#if MIN_VERSION_tls(1,5,0)
                    TLS.TLS13 -> (3,4)
#endif
            clientCert <- TLS.getClientCertificateChain ctx
            return TLS {
                tlsMajorVersion = major
              , tlsMinorVersion = minor
              , tlsNegotiatedProtocol = proto
              , tlsChiperID = TLS.cipherID infoCipher
              , tlsClientCertificate = clientCert
              }

tryIO :: IO a -> IO (Either IOException a)
tryIO = try

----------------------------------------------------------------

plainHTTP :: TLSSettings -> Settings -> Socket -> S.ByteString -> IO (Connection, Transport)
plainHTTP TLSSettings{..} set s bs0 = case onInsecure of
    AllowInsecure -> do
        conn' <- socketConnection set s
        cachedRef <- I.newIORef bs0
        let conn'' = conn'
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
        sendAll s "HTTP/1.1 426 Upgrade Required\
        \r\nUpgrade: TLS/1.0, HTTP/1.1\
        \r\nConnection: Upgrade\
        \r\nContent-Type: text/plain\r\n\r\n"
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
