{-# LANGUAGE CPP #-}

module Network.Wai.Handler.WarpTLS.Internal (
      CertSettings(..)
    , TLSSettings(..)
    , OnInsecure(..)
    -- * Accessors
    , getCertSettings
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.IORef as I
import qualified Network.TLS as TLS
import qualified Network.TLS.SessionManager as SM

----------------------------------------------------------------

-- | Determines where to load the certificate, chain
-- certificates, and key from.
data CertSettings
  = CertFromFile !FilePath ![FilePath] !FilePath
  | CertFromMemory !S.ByteString ![S.ByteString] !S.ByteString
  | CertFromRef !(I.IORef S.ByteString) ![I.IORef S.ByteString] !(I.IORef S.ByteString)

----------------------------------------------------------------

-- | An action when a plain HTTP comes to HTTP over TLS/SSL port.
data OnInsecure = DenyInsecure L.ByteString
                | AllowInsecure
                deriving (Show)

----------------------------------------------------------------

-- | Settings for WarpTLS.
data TLSSettings = TLSSettings {
    certSettings :: CertSettings
    -- ^ Where are the certificate, chain certificates, and key
    -- loaded from?
    --
    -- >>> certSettings defaultTlsSettings
    -- tlsSettings "certificate.pem" "key.pem"
    --
    -- @since 3.3.0
  , onInsecure :: OnInsecure
    -- ^ Do we allow insecure connections with this server as well?
    --
    -- >>> onInsecure defaultTlsSettings
    -- DenyInsecure "This server only accepts secure HTTPS connections."
    --
    -- Since 1.4.0
  , tlsLogging :: TLS.Logging
    -- ^ The level of logging to turn on.
    --
    -- Default: 'TLS.defaultLogging'.
    --
    -- Since 1.4.0
  , tlsAllowedVersions :: [TLS.Version]
#if MIN_VERSION_tls(1,5,0)
    -- ^ The TLS versions this server accepts.
    --
    -- >>> tlsAllowedVersions defaultTlsSettings
    -- [TLS13,TLS12,TLS11,TLS10]
    --
    -- Since 1.4.2
#else
    -- ^ The TLS versions this server accepts.
    --
    -- >>> tlsAllowedVersions defaultTlsSettings
    -- [TLS12,TLS11,TLS10]
    --
    -- Since 1.4.2
#endif
  , tlsCiphers :: [TLS.Cipher]
#if MIN_VERSION_tls(1,5,0)
    -- ^ The TLS ciphers this server accepts.
    --
    -- >>> tlsCiphers defaultTlsSettings
    -- [ECDHE-ECDSA-AES256GCM-SHA384,ECDHE-ECDSA-AES128GCM-SHA256,ECDHE-RSA-AES256GCM-SHA384,ECDHE-RSA-AES128GCM-SHA256,DHE-RSA-AES256GCM-SHA384,DHE-RSA-AES128GCM-SHA256,ECDHE-ECDSA-AES256CBC-SHA384,ECDHE-RSA-AES256CBC-SHA384,DHE-RSA-AES256-SHA256,ECDHE-ECDSA-AES256CBC-SHA,ECDHE-RSA-AES256CBC-SHA,DHE-RSA-AES256-SHA1,RSA-AES256GCM-SHA384,RSA-AES256-SHA256,RSA-AES256-SHA1,AES128GCM-SHA256,AES256GCM-SHA384]
    --
    -- Since 1.4.2
#else
    -- ^ The TLS ciphers this server accepts.
    --
    -- >>> tlsCiphers defaultTlsSettings
    -- [ECDHE-ECDSA-AES256GCM-SHA384,ECDHE-ECDSA-AES128GCM-SHA256,ECDHE-RSA-AES256GCM-SHA384,ECDHE-RSA-AES128GCM-SHA256,DHE-RSA-AES256GCM-SHA384,DHE-RSA-AES128GCM-SHA256,ECDHE-ECDSA-AES256CBC-SHA384,ECDHE-RSA-AES256CBC-SHA384,DHE-RSA-AES256-SHA256,ECDHE-ECDSA-AES256CBC-SHA,ECDHE-RSA-AES256CBC-SHA,DHE-RSA-AES256-SHA1,RSA-AES256GCM-SHA384,RSA-AES256-SHA256,RSA-AES256-SHA1]
    --
    -- Since 1.4.2
#endif
  , tlsWantClientCert :: Bool
    -- ^ Whether or not to demand a certificate from the client.  If this
    -- is set to True, you must handle received certificates in a server hook
    -- or all connections will fail.
    --
    -- >>> tlsWantClientCert defaultTlsSettings
    -- False
    --
    -- Since 3.0.2
  , tlsServerHooks :: TLS.ServerHooks
    -- ^ The server-side hooks called by the tls package, including actions
    -- to take when a client certificate is received.  See the "Network.TLS"
    -- module for details.
    --
    -- Default: def
    --
    -- Since 3.0.2
  , tlsServerDHEParams :: Maybe TLS.DHParams
    -- ^ Configuration for ServerDHEParams
    -- more function lives in `crypton` package
    --
    -- Default: Nothing
    --
    -- Since 3.2.2
  , tlsSessionManagerConfig :: Maybe SM.Config
    -- ^ Configuration for in-memory TLS session manager.
    -- If Nothing, 'TLS.noSessionManager' is used.
    -- Otherwise, an in-memory TLS session manager is created
    -- according to 'Config'.
    --
    -- Default: Nothing
    --
    -- Since 3.2.4
  , tlsCredentials :: Maybe TLS.Credentials
    -- ^ Specifying 'TLS.Credentials' directly.  If this value is
    --   specified, other fields such as 'certFile' are ignored.
    --
    --   Since 3.2.12
  , tlsSessionManager :: Maybe TLS.SessionManager
    -- ^ Specifying 'TLS.SessionManager' directly. If this value is
    --   specified, 'tlsSessionManagerConfig' is ignored.
    --
    --   Since 3.2.12
  , tlsSupportedHashSignatures :: [TLS.HashAndSignatureAlgorithm]
    -- ^ Specifying supported hash/signature algorithms, ordered by decreasing
    -- priority. See the "Network.TLS" module for details
    --
    --   Since 3.3.3
  }


-- Since 3.3.1
-- | Some programs need access to cert settings
getCertSettings :: TLSSettings -> CertSettings
getCertSettings tlsSetgs = certSettings tlsSetgs
