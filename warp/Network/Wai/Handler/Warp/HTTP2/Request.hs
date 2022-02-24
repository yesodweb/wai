{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.HTTP2.Request (
    toRequest
  , getHTTP2Data
  , setHTTP2Data
  , modifyHTTP2Data
  ) where

import Control.Arrow (first)
import qualified Data.ByteString.Char8 as C8
import Data.IORef
import qualified Data.Vault.Lazy as Vault
import Network.HPACK
import Network.HPACK.Token
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Internal (Request(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified System.TimeManager as T

import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Request (getFileInfoKey, pauseTimeoutKey)
#ifdef MIN_VERSION_x509
import Network.Wai.Handler.Warp.Request (getClientCertificateKey)
#endif
import qualified Network.Wai.Handler.Warp.Settings as S (Settings, settingsNoParsePath)
import Network.Wai.Handler.Warp.Types

type ToReq = (TokenHeaderList,ValueTable) -> Maybe Int -> IO ByteString -> T.Handle -> Transport -> IO Request

----------------------------------------------------------------

http30 :: H.HttpVersion
http30 = H.HttpVersion 3 0

toRequest :: InternalInfo -> S.Settings -> SockAddr -> ToReq
toRequest ii settings addr ht bodylen body th transport = do
    ref <- newIORef Nothing
    toRequest' ii settings addr ref ht bodylen body th transport

toRequest' :: InternalInfo -> S.Settings -> SockAddr
           -> IORef (Maybe HTTP2Data)
           -> ToReq
toRequest' ii settings addr ref (reqths,reqvt) bodylen body th transport = return req
  where
    !req = Request {
        requestMethod = colonMethod
      , httpVersion = if isTransportQUIC transport then http30 else H.http20
      , rawPathInfo = rawPath
      , pathInfo = H.decodePathSegments path
      , rawQueryString = query
      , queryString = H.parseQuery query
      , requestHeaders = headers
      , isSecure = isTransportSecure transport
      , remoteHost = addr
      , requestBody = body
      , vault = vaultValue
      , requestBodyLength = maybe ChunkedBody (KnownLength . fromIntegral) bodylen
      , requestHeaderHost      = mHost <|> mAuth
      , requestHeaderRange     = mRange
      , requestHeaderReferer   = mReferer
      , requestHeaderUserAgent = mUserAgent
      }
    headers = map (first tokenKey) ths
      where
        ths = case mHost of
            Just _  -> reqths
            Nothing -> case mAuth of
              Just auth -> (tokenHost, auth) : reqths
              _         -> reqths
    !mPath = getHeaderValue tokenPath reqvt -- SHOULD
    !colonMethod = fromJust $ getHeaderValue tokenMethod reqvt -- MUST
    !mAuth = getHeaderValue tokenAuthority reqvt -- SHOULD
    !mHost = getHeaderValue tokenHost reqvt
    !mRange = getHeaderValue tokenRange reqvt
    !mReferer = getHeaderValue tokenReferer reqvt
    !mUserAgent = getHeaderValue tokenUserAgent reqvt
    -- CONNECT request will have ":path" omitted, use ":authority" as unparsed
    -- path instead so that it will have consistent behavior compare to HTTP 1.0
    (unparsedPath,query) = C8.break (=='?') $ fromJust (mPath <|> mAuth)
    !path = H.extractPath unparsedPath
    !rawPath = if S.settingsNoParsePath settings then unparsedPath else path
    -- fixme: pauseTimeout. th is not available here.
    !vaultValue = Vault.insert getFileInfoKey (getFileInfo ii)
                $ Vault.insert getHTTP2DataKey (readIORef ref)
                $ Vault.insert setHTTP2DataKey (writeIORef ref)
                $ Vault.insert modifyHTTP2DataKey (modifyIORef' ref)
                $ Vault.insert pauseTimeoutKey (T.pause th)
#ifdef MIN_VERSION_x509
                $ Vault.insert getClientCertificateKey (getTransportClientCertificate transport)
#endif
                  Vault.empty

getHTTP2DataKey :: Vault.Key (IO (Maybe HTTP2Data))
getHTTP2DataKey = unsafePerformIO Vault.newKey
{-# NOINLINE getHTTP2DataKey #-}

-- | Getting 'HTTP2Data' through vault of the request.
--   Warp uses this to receive 'HTTP2Data' from 'Middleware'.
--
--   Since: 3.2.7
getHTTP2Data :: Request -> IO (Maybe HTTP2Data)
getHTTP2Data req = case Vault.lookup getHTTP2DataKey (vault req) of
  Nothing     -> return Nothing
  Just getter -> getter

setHTTP2DataKey :: Vault.Key (Maybe HTTP2Data -> IO ())
setHTTP2DataKey = unsafePerformIO Vault.newKey
{-# NOINLINE setHTTP2DataKey #-}

-- | Setting 'HTTP2Data' through vault of the request.
--   'Application' or 'Middleware' should use this.
--
--   Since: 3.2.7
setHTTP2Data :: Request -> Maybe HTTP2Data -> IO ()
setHTTP2Data req mh2d = case Vault.lookup setHTTP2DataKey (vault req) of
  Nothing     -> return ()
  Just setter -> setter mh2d

modifyHTTP2DataKey :: Vault.Key ((Maybe HTTP2Data -> Maybe HTTP2Data) -> IO ())
modifyHTTP2DataKey = unsafePerformIO Vault.newKey
{-# NOINLINE modifyHTTP2DataKey #-}

-- | Modifying 'HTTP2Data' through vault of the request.
--   'Application' or 'Middleware' should use this.
--
--   Since: 3.2.8
modifyHTTP2Data :: Request -> (Maybe HTTP2Data -> Maybe HTTP2Data) -> IO ()
modifyHTTP2Data req func = case Vault.lookup modifyHTTP2DataKey (vault req) of
  Nothing     -> return ()
  Just modify -> modify func
