{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Handler.WarpTLS
    ( TLSSettings (..)
    , runTLS
    ) where

import Network.TLS
import Network.Wai.Handler.Warp
import Network.Wai
import Network.Socket
import System.IO
import Crypto.Random
import qualified Data.ByteString.Lazy as L
import qualified Data.Enumerator.List as EL
import Data.Enumerator.Binary (enumFileRange)
import Data.Enumerator (run_, ($$))
import Control.Monad.IO.Class (liftIO)
import Control.Exception (bracket)
import qualified Data.ByteString as S
import Network.TLS.Extra
import qualified Crypto.Cipher.RSA as RSA
import qualified Data.Certificate.KeyRSA as KeyRSA
import Data.Certificate.X509
import Data.Certificate.PEM
import qualified Data.ByteString as B

data TLSSettings = TLSSettings
    { certFile :: FilePath
    , keyFile :: FilePath
    }

runTLS :: TLSSettings -> Settings -> Application -> IO ()
runTLS tset set app = do
    cert    <- readCertificate $ certFile tset
    pk      <- readPrivateKey $ keyFile tset
    let params = defaultParams
            { pWantClientCert = False
            , pAllowedVersions = [SSL3,TLS10,TLS11,TLS12]
            , pCiphers         = ciphers
            , pCertificates    = [(cert, Just pk)]
            }
    bracket
        (bindPort (settingsPort set) (settingsHost set))
        sClose
        (\sock -> runSettingsConnection set (getter params sock) app)
  where
    getter params sock = do
        (s, sa) <- accept sock
        h <- socketToHandle s ReadWriteMode
        hSetBuffering h NoBuffering
        gen <- newGenIO
        ctx <- server params (gen :: SystemRandom) h
        handshake ctx
        let sink = do
                x <- EL.head
                case x of
                    Just y -> do
                        liftIO $ sendData ctx $ L.fromChunks [y]
                        sink
                    Nothing -> return ()
        let conn = Connection
                { connSendMany = sendData ctx . L.fromChunks
                , connSendAll = sendData ctx . L.fromChunks . return
                , connSendFile = \fp offset length _th -> run_ $ enumFileRange fp (Just offset) (Just length) $$ EL.mapM_ (sendData ctx . L.fromChunks . return)
                , connIter = \_th -> sink
                , connClose = do
                    bye ctx
                    hClose h
                , connEnum = \_th -> EL.generateM $ do
                    lbs <- recvData ctx
                    if L.null lbs
                        then return Nothing
                        else return $ Just $ S.concat $ L.toChunks lbs
                }
        return (conn, sa)

-- taken from stunnel example in tls-extra
ciphers :: [Cipher]
ciphers =
	[ cipher_AES128_SHA1
	, cipher_AES256_SHA1
	, cipher_RC4_128_MD5
	, cipher_RC4_128_SHA1
	]

readCertificate :: FilePath -> IO X509
readCertificate filepath = do
	content <- B.readFile filepath
	let certdata = case parsePEMCert content of
		Nothing -> error ("no valid certificate section")
		Just x  -> x
	let cert = case decodeCertificate $ L.fromChunks [certdata] of
		Left err -> error ("cannot decode certificate: " ++ err)
		Right x  -> x
	return cert

readPrivateKey :: FilePath -> IO PrivateKey
readPrivateKey filepath = do
	content <- B.readFile filepath
	let pkdata = case parsePEMKeyRSA content of
		Nothing -> error ("no valid RSA key section")
		Just x  -> L.fromChunks [x]
	let pk = case KeyRSA.decodePrivate pkdata of
		Left err -> error ("cannot decode key: " ++ err)
		Right x  -> PrivRSA $ RSA.PrivateKey
			{ RSA.private_sz   = fromIntegral $ KeyRSA.lenmodulus x
			, RSA.private_n    = KeyRSA.modulus x
			, RSA.private_d    = KeyRSA.private_exponant x
			, RSA.private_p    = KeyRSA.p1 x
			, RSA.private_q    = KeyRSA.p2 x
			, RSA.private_dP   = KeyRSA.exp1 x
			, RSA.private_dQ   = KeyRSA.exp2 x
			, RSA.private_qinv = KeyRSA.coef x
			}
	return pk
