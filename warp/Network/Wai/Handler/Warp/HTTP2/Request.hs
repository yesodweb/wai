{-# LANGUAGE OverloadedStrings, CPP #-}
{-# LANGUAGE BangPatterns #-}

module Network.Wai.Handler.Warp.HTTP2.Request (
    mkRequest
  , MkReq
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import Data.Maybe (fromJust)
import qualified Data.Vault.Lazy as Vault
import Network.HPACK
import Network.HPACK.Token
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.HashMap (hashByteString)
import Network.Wai.Handler.Warp.Request (pauseTimeoutKey, getFileInfoKey)
import qualified Network.Wai.Handler.Warp.Settings as S (Settings, settingsNoParsePath)
import qualified Network.Wai.Handler.Warp.Timeout as Timeout
import Network.Wai.Handler.Warp.Types
import Network.Wai.Internal (Request(..))

type MkReq = (TokenHeaderList,ValueTable) -> IO ByteString -> (Request,InternalInfo)

mkRequest :: InternalInfo1 -> S.Settings -> SockAddr -> MkReq
mkRequest ii1 settings addr (reqths,reqvt) body = (req,ii)
  where
    !req = Request {
        requestMethod = colonMethod
      , httpVersion = http2ver
      , rawPathInfo = rawPath
      , pathInfo = H.decodePathSegments path
      , rawQueryString = query
      , queryString = H.parseQuery query
      , requestHeaders = headers
      , isSecure = True
      , remoteHost = addr
      , requestBody = body
      , vault = vaultValue
      , requestBodyLength = ChunkedBody -- fixme
      , requestHeaderHost      = mHost
      , requestHeaderRange     = mRange
      , requestHeaderReferer   = mReferer
      , requestHeaderUserAgent = mUserAgent
      }
    headers = map (first tokenKey) ths
      where
        ths = case mHost of
            Nothing -> (tokenHost, colonAuth) : reqths
            Just _  -> reqths
    !colonPath = fromJust $ getHeaderValue tokenPath reqvt
    !colonMethod = fromJust $ getHeaderValue tokenMethod reqvt
    !mAuth = getHeaderValue tokenAuthority reqvt
    !colonAuth = fromJust $ mAuth
    !mHost = getHeaderValue tokenHost reqvt <|> mAuth
    !mRange = getHeaderValue tokenRange reqvt
    !mReferer = getHeaderValue tokenReferer reqvt
    !mUserAgent = getHeaderValue tokenUserAgent reqvt
    (unparsedPath,query) = B8.break (=='?') colonPath
    !path = H.extractPath unparsedPath
    !rawPath = if S.settingsNoParsePath settings then unparsedPath else path
    !h = hashByteString rawPath
    !ii = toInternalInfo ii1 h
    !th = threadHandle ii
    !vaultValue = Vault.insert pauseTimeoutKey (Timeout.pause th)
                $ Vault.insert getFileInfoKey (getFileInfo ii)
                  Vault.empty
