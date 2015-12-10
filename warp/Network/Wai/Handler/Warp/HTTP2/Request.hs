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
    vhMethod :: !ByteString
  , vhPath   :: !ByteString
  , vhAuth   :: !(Maybe ByteString)
  , vhCL     :: !(Maybe Int)
  , vhHeader :: !RequestHeaders
  } deriving Show

type MkReq = ValidHeaders -> IO ByteString -> Request

mkRequest :: InternalInfo -> S.Settings -> SockAddr -> MkReq
mkRequest ii settings addr (ValidHeaders m p ma _ hdr) body = req
  where
    (unparsedPath,query) = B8.break (=='?') p
    !path = H.extractPath unparsedPath
    !req = Request {
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
    !th = threadHandle ii
    !vaultValue = Vault.insert pauseTimeoutKey (Timeout.pause th)
               $ Vault.insert getFileInfoKey (fileInfo ii)
                 Vault.empty

----------------------------------------------------------------

data Pseudo = Pseudo {
    colonMethod :: !(Maybe ByteString)
  , colonPath   :: !(Maybe ByteString)
  , colonAuth   :: !(Maybe ByteString)
  , contentLen  :: !(Maybe ByteString)
  } deriving Show

emptyPseudo :: Pseudo
emptyPseudo = Pseudo Nothing Nothing Nothing Nothing

-- |
--
-- >>> validateHeaders [(":method","GET"),(":path","path")]
-- Just (ValidHeaders {vhMethod = "GET", vhPath = "path", vhAuth = Nothing, vhCL = Nothing, vhHeader = []})
-- >>> validateHeaders [(":method","GET"),(":path","path"),(":authority","authority"),("accept-language","en")]
-- Just (ValidHeaders {vhMethod = "GET", vhPath = "path", vhAuth = Just "authority", vhCL = Nothing, vhHeader = [("accept-language","en")]})
-- >>> validateHeaders [(":method","GET"),(":path","path"),("cookie","a=b"),("accept-language","en"),("cookie","c=d"),("cookie","e=f")]
-- Just (ValidHeaders {vhMethod = "GET", vhPath = "path", vhAuth = Nothing, vhCL = Nothing, vhHeader = [("accept-language","en"),("cookie","a=b; c=d; e=f")]})
validateHeaders :: HeaderList -> Maybe ValidHeaders
validateHeaders hs = case pseudo hs emptyPseudo of
    Just (Pseudo (Just m) (Just p) ma mcl, !h)
        -> Just $! ValidHeaders m p ma (readInt <$> mcl) h
    _   -> Nothing
  where
    pseudo [] !p          = Just (p,[])
    pseudo h@((k,v):kvs) !p
      | k == ":method"    = if isJust (colonMethod p) then
                                Nothing
                              else
                                pseudo kvs (p { colonMethod = Just v })
      | k == ":path"      = if isJust (colonPath p) then
                                Nothing
                              else
                                pseudo kvs (p { colonPath   = Just v })
      | k == ":authority" = if isJust (colonAuth p) then
                                Nothing
                              else
                                pseudo kvs (p { colonAuth   = Just v })
      | k == ":scheme"    = pseudo kvs p -- fixme: how to store :scheme?
      | isPseudo k        = Nothing
      | otherwise         = normal h (p,id,id)

    normal [] (!p,b,c)     = Just (p, mkH b c)
    normal ((k,v):kvs) (!p,b,c)
      | isPseudo k        = Nothing
      | k == "connection" = Nothing
      | k == "te"         = if v == "trailers" then
                                normal kvs (p, b . ((mk k,v) :), c)
                              else
                                Nothing
      | k == "content-length"
                          = normal kvs (p { contentLen = Just v }, b . ((mk k,v) :), c)
      | k == "host"       = if isJust (colonAuth p) then
                                normal kvs (p, b, c)
                              else
                                normal kvs (p { colonAuth = Just v }, b, c)
      | k == "cookie"     = normal kvs (p, b, c . (v:))
      | otherwise         = case BS.find isUpper k of
                                 Nothing -> normal kvs (p, b . ((mk k,v) :), c)
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
