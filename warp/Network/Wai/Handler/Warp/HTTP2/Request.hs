{-# LANGUAGE OverloadedStrings, CPP #-}
{-# LANGUAGE BangPatterns #-}

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
import Network.HTTP.Types (RequestHeaders)
import qualified Network.HTTP.Types as H
import Network.Socket (SockAddr)
import Network.Wai
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.HashMap (hashByteString)
import Network.Wai.Handler.Warp.ReadInt
import Network.Wai.Handler.Warp.Request (pauseTimeoutKey, getFileInfoKey)
import Network.Wai.Handler.Warp.Types
import Network.Wai.Internal (Request(..))
import qualified Network.Wai.Handler.Warp.Settings as S (Settings, settingsNoParsePath)
import qualified Network.Wai.Handler.Warp.Timeout as Timeout

data ValidHeaders = ValidHeaders {
    vhMethod  :: !ByteString
  , vhPath    :: !ByteString
  , vhAuth    :: !(Maybe ByteString)
  , vhRange   :: !(Maybe ByteString)
  , vhReferer :: !(Maybe ByteString)
  , vhUA      :: !(Maybe ByteString)
  , vhCL      :: !(Maybe Int)
  , vhHeader  :: !RequestHeaders
  } deriving Show

type MkReq = ValidHeaders -> IO ByteString -> (Request,InternalInfo)

mkRequest :: InternalInfo1 -> S.Settings -> SockAddr -> MkReq
mkRequest ii1 settings addr (ValidHeaders m p ma mrng mrr mua _ hdr) body =
    (req,ii)
  where
    (unparsedPath,query) = B8.break (=='?') p
    !path = H.extractPath unparsedPath
    !rawPath = if S.settingsNoParsePath settings then unparsedPath else path
    !h = hashByteString rawPath
    !req = Request {
        requestMethod = m
      , httpVersion = http2ver
      , rawPathInfo = rawPath
      , pathInfo = H.decodePathSegments path
      , rawQueryString = query
      , queryString = H.parseQuery query
      , requestHeaders = case ma of
                           Nothing -> hdr
                           Just hv  -> (mk "host", hv) : hdr
      , isSecure = True
      , remoteHost = addr
      , requestBody = body
      , vault = vaultValue
      , requestBodyLength = ChunkedBody -- fixme
      , requestHeaderHost      = ma
      , requestHeaderRange     = mrng
      , requestHeaderReferer   = mrr
      , requestHeaderUserAgent = mua
      }
    !ii = toInternalInfo ii1 h
    !th = threadHandle ii
    !vaultValue = Vault.insert pauseTimeoutKey (Timeout.pause th)
                $ Vault.insert getFileInfoKey (getFileInfo ii)
                  Vault.empty

----------------------------------------------------------------

data Special = Special {
    colonMethod :: !(Maybe ByteString)
  , colonPath   :: !(Maybe ByteString)
  , colonAuth   :: !(Maybe ByteString)
  , sRange      :: !(Maybe ByteString)
  , sReferer    :: !(Maybe ByteString)
  , sUA         :: !(Maybe ByteString)
  , contentLen  :: !(Maybe ByteString)
  } deriving Show

emptySpecial :: Special
emptySpecial = Special Nothing Nothing Nothing Nothing Nothing Nothing Nothing

-- |
--
-- >>> validateHeaders [(":method","GET"),(":path","path")]
-- Just (ValidHeaders {vhMethod = "GET", vhPath = "path", vhAuth = Nothing, vhRange = Nothing, vhReferer = Nothing, vhUA = Nothing, vhCL = Nothing, vhHeader = []})
-- >>> validateHeaders [(":method","GET"),(":path","path"),(":authority","authority"),("accept-language","en")]
-- Just (ValidHeaders {vhMethod = "GET", vhPath = "path", vhAuth = Just "authority", vhRange = Nothing, vhReferer = Nothing, vhUA = Nothing, vhCL = Nothing, vhHeader = [("accept-language","en")]})
-- >>> validateHeaders [(":method","GET"),(":path","path"),("cookie","a=b"),("accept-language","en"),("cookie","c=d"),("cookie","e=f")]
-- Just (ValidHeaders {vhMethod = "GET", vhPath = "path", vhAuth = Nothing, vhRange = Nothing, vhReferer = Nothing, vhUA = Nothing, vhCL = Nothing, vhHeader = [("accept-language","en"),("cookie","a=b; c=d; e=f")]})
validateHeaders :: HeaderList -> Maybe ValidHeaders
validateHeaders hs = case pseudo hs emptySpecial of
    Just (Special (Just m) (Just p) ma mrng mrr mua mcl, !h)
        -> Just $! ValidHeaders m p ma mrng mrr mua (readInt <$> mcl) h
    _   -> Nothing
  where
    pseudo [] !s          = Just (s,[])
    pseudo h@((k,v):kvs) !s
      | k == ":method"    = if isJust (colonMethod s) then
                                Nothing
                              else
                                pseudo kvs (s { colonMethod = Just v })
      | k == ":path"      = if isJust (colonPath s) then
                                Nothing
                              else
                                pseudo kvs (s { colonPath   = Just v })
      | k == ":authority" = if isJust (colonAuth s) then
                                Nothing
                              else
                                pseudo kvs (s { colonAuth   = Just v })
      | k == ":scheme"    = pseudo kvs s -- fixme: how to store :scheme?
      | isPseudo k        = Nothing
      | otherwise         = normal h (s,id,id)

    normal [] (!s,b,c)     = Just (s, mkH b c)
    normal ((k,v):kvs) (!s,b,c)
      | isPseudo k        = Nothing
      | k == "connection" = Nothing
      | k == "te"         = if v == "trailers" then
                                normal kvs (s, b . ((mk k,v) :), c)
                              else
                                Nothing
      | k == "range"
                          = normal kvs (s {sRange = Just v }, b . ((mk k,v) :), c)
      | k == "referer"
                          = normal kvs (s { sReferer = Just v }, b . ((mk k,v) :), c)
      | k == "user-agent"
                          = normal kvs (s { sUA = Just v }, b . ((mk k,v) :), c)
      | k == "content-length"
                          = normal kvs (s { contentLen = Just v }, b . ((mk k,v) :), c)
      | k == "host"       = if isJust (colonAuth s) then
                                normal kvs (s, b, c)
                              else
                                normal kvs (s { colonAuth = Just v }, b, c)
      | k == "cookie"     = normal kvs (s, b, c . (v:))
      | otherwise         = case BS.find isUpper k of
                                 Nothing -> normal kvs (s, b . ((mk k,v) :), c)
                                 Just _  -> Nothing

    mkH b c = h
      where
        !h = b anchor
        !cookieList = c []
        !anchor
          | null cookieList = []
          | otherwise       = let !v = BS.intercalate "; " cookieList
                              in [("cookie",v)]
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
