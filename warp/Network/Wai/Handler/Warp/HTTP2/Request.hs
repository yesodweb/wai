{-# LANGUAGE OverloadedStrings, CPP #-}

module Network.Wai.Handler.Warp.HTTP2.Request (
    mkRequest
  , newReadBody
  , MkReq
  , ValidHeaders(..)
  , validateHeaders
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative ((<$>))
#endif
import Control.Concurrent.STM
import Control.Monad (when)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.CaseInsensitive (mk)
import Data.IORef (IORef, readIORef, newIORef, writeIORef)
import Data.Maybe (isJust)
import qualified Data.Vault.Lazy as Vault
#if __GLASGOW_HASKELL__ < 709
import Data.Monoid (mempty)
#endif
import Data.Word8 (isUpper,_colon)
import Network.HPACK
import Network.HTTP.Types (RequestHeaders,hRange)
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Internal (Request(..))
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.ReadInt
import Network.Wai.Handler.Warp.Request (pauseTimeoutKey, getFileInfoKey)
import Network.Wai.Handler.Warp.Types (InternalInfo(..))
import qualified Network.Wai.Handler.Warp.Settings as S (Settings, settingsNoParsePath)
import qualified Network.Wai.Handler.Warp.Timeout as Timeout

data ValidHeaders = ValidHeaders {
    vhMethod :: ByteString
  , vhPath   :: ByteString
  , vhAuth   :: Maybe ByteString
  , vhCL     :: Maybe Int
  , vhHeader :: RequestHeaders
  }

type MkReq = ValidHeaders -> IO ByteString -> Request

mkRequest :: InternalInfo -> S.Settings -> SockAddr -> MkReq
mkRequest ii settings addr (ValidHeaders m p ma _ hdr) body = req
  where
    (unparsedPath,query) = B8.break (=='?') p
    path = H.extractPath unparsedPath
    req = Request {
        requestMethod = m
      , httpVersion = http2ver
      , rawPathInfo = if S.settingsNoParsePath settings then unparsedPath else path
      , pathInfo = H.decodePathSegments path
      , rawQueryString = query
      , queryString = H.parseQuery query
      , requestHeaders = case ma of
                           Nothing -> hdr
                           Just h  -> (mk "host", h) : hdr
      , isSecure = True
      , remoteHost = addr
      , requestBody = body
      , vault = vaultValue
      , requestBodyLength = ChunkedBody -- fixme
      , requestHeaderHost = ma
      , requestHeaderRange = lookup hRange hdr
      }
    th = threadHandle ii
    vaultValue = Vault.insert pauseTimeoutKey (Timeout.pause th)
               $ Vault.insert getFileInfoKey (fileInfo ii)
                 Vault.empty

----------------------------------------------------------------

data Pseudo = Pseudo {
    colonMethod :: !(Maybe ByteString)
  , colonPath   :: !(Maybe ByteString)
  , colonAuth   :: !(Maybe ByteString)
  , contentLen  :: !(Maybe ByteString)
  }

emptyPseudo :: Pseudo
emptyPseudo = Pseudo Nothing Nothing Nothing Nothing

validateHeaders :: HeaderList -> Maybe ValidHeaders
validateHeaders hs = case pseudo hs (emptyPseudo,id) of
    Just (Pseudo (Just m) (Just p) ma mcl, h)
        -> Just $ ValidHeaders m p ma (readInt <$> mcl) h
    _   -> Nothing
  where
    pseudo [] (p,b)       = Just (p,b [])
    pseudo h@((k,v):kvs) (p,b)
      | k == ":method"    = if isJust (colonMethod p) then
                                Nothing
                              else
                                pseudo kvs (p { colonMethod = Just v },b)
      | k == ":path"      = if isJust (colonPath p) then
                                Nothing
                              else
                                pseudo kvs (p { colonPath   = Just v },b)
      | k == ":authority" = if isJust (colonAuth p) then
                                Nothing
                              else
                                pseudo kvs (p { colonAuth   = Just v },b)
      | k == ":scheme"    = pseudo kvs (p,b) -- fixme: how to store :scheme?
      | isPseudo k        = Nothing
      | otherwise         = normal h (p,b)

    normal [] (p,b)       = Just (p,b [])
    normal ((k,v):kvs) (p,b)
      | isPseudo k        = Nothing
      | k == "connection" = Nothing
      | k == "te"         = if v == "trailers" then
                                normal kvs (p, b . ((mk k,v) :))
                              else
                                Nothing
      | k == "content-length"
                          = normal kvs (p { contentLen = Just v }, b . ((mk k,v) :))
      | k == "host"       = if isJust (colonAuth p) then
                                normal kvs (p,b)
                              else
                                normal kvs (p { colonAuth = Just v },b)
      | otherwise         = case BS.find isUpper k of
                                 Nothing -> normal kvs (p, b . ((mk k,v) :))
                                 Just _  -> Nothing

    isPseudo "" = False
    isPseudo k  = BS.head k == _colon


----------------------------------------------------------------

newReadBody :: TQueue ByteString -> IO (IO ByteString)
newReadBody q = do
    ref <- newIORef False
    return $ readBody q ref

readBody :: TQueue ByteString -> IORef Bool -> IO ByteString
readBody q ref = do
    eof <- readIORef ref
    if eof then
        return ""
      else do
        bs <- atomically $ readTQueue q
        when (bs == "") $ writeIORef ref True
        return bs
