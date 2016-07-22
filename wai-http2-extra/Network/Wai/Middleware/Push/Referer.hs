{-# LANGUAGE BangPatterns, OverloadedStrings, RecordWildCards #-}

-- | Middleware for server push learning dependency based on Referer:.
module Network.Wai.Middleware.Push.Referer (
  -- * Middleware
    pushOnReferer
  -- * Making push promise
  , URLPath
  , MakePushPromise
  , defaultMakePushPromise
  -- * Settings
  , Settings
  , defaultSettings
  , makePushPromise
  , duration
  , keyLimit
  , valueLimit
  ) where

import Control.Monad (when, unless)
import Control.Reaper
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.ByteString.Internal (ByteString(..), memchr)
import Data.IORef
import Data.Maybe (isNothing)
import Data.Word (Word8)
import Data.Word8
import Foreign.ForeignPtr (withForeignPtr, ForeignPtr)
import Foreign.Ptr (Ptr, plusPtr, minusPtr, nullPtr)
import Foreign.Storable (peek)
import Network.HTTP.Types (Status(..))
import Network.Wai
import Network.Wai.Handler.Warp hiding (Settings, defaultSettings)
import Network.Wai.Internal (Response(..))
import System.IO.Unsafe (unsafePerformIO)

import qualified Network.Wai.Middleware.Push.Referer.LimitMultiMap as M

-- $setup
-- >>> :set -XOverloadedStrings

-- | Making a push promise based on Referer:,
--   path to be pushed and file to be pushed.
--   If the middleware should push this file in the next time when
--   the page of Referer: is accessed,
--   this function should return 'Just'.
--   If 'Nothing' is returned,
--   the middleware learns nothing.
type MakePushPromise = URLPath  -- ^ path in referer  (key: /index.html)
                    -> URLPath  -- ^ path to be pushed (value: /style.css)
                    -> FilePath -- ^ file to be pushed (file_path/style.css)
                    -> IO (Maybe PushPromise)

-- | Type for URL path.
type URLPath = ByteString

type Cache = M.LimitMultiMap URLPath PushPromise

initialized :: IORef Bool
initialized = unsafePerformIO $ newIORef False
{-# NOINLINE initialized #-}

cacheReaper :: IORef (Maybe (Reaper Cache (URLPath,PushPromise)))
cacheReaper = unsafePerformIO $ newIORef Nothing
{-# NOINLINE cacheReaper #-}

-- | Settings for server push based on Referer:.
data Settings = Settings {
    makePushPromise :: MakePushPromise -- ^ Default: 'defaultMakePushPromise'
  , duration :: Int -- ^ Duration (in micro seconds) to keep the learning information. The information completely cleared every this duration. Default: 30000000
  , keyLimit :: Int -- ^ Max number of keys (e.g. index.html) in the learning information. Default: 20
  , valueLimit :: Int -- ^ Max number of values (e.g. style.css) in the learning information. Default: 20
  }

-- | Default settings.
defaultSettings :: Settings
defaultSettings = Settings {
    makePushPromise = defaultMakePushPromise
  , duration = 30000000
  , keyLimit = 20
  , valueLimit = 20
  }

tryInitialize :: Settings -> IO ()
tryInitialize Settings{..} = do
    isInitialized <- atomicModifyIORef' initialized $ \x -> (True, x)
    unless isInitialized $ do
        reaper <- mkReaper settings
        writeIORef cacheReaper (Just reaper)
  where
    emptyCache = M.empty keyLimit valueLimit
    settings :: ReaperSettings Cache (URLPath,PushPromise)
    settings = defaultReaperSettings {
        reaperAction = \_ -> return (\_ -> emptyCache)
      , reaperCons   = M.insert
      , reaperNull   = M.isEmpty
      , reaperEmpty  = emptyCache
      , reaperDelay  = duration
      }

-- | The middleware to push files based on Referer:.
--   Learning strategy is implemented in the first argument.
pushOnReferer :: Settings -> Middleware
pushOnReferer settings@Settings{..} app req sendResponse = do
    tryInitialize settings
    mreaper <- readIORef cacheReaper
    case mreaper of
        Nothing     -> app req sendResponse
        Just reaper -> app req (push reaper)
  where
    push reaper res@(ResponseFile (Status 200 "OK") _ file Nothing) = do
        let !path = rawPathInfo req
        m <- reaperRead reaper
        case M.lookup path m of
            [] -> case requestHeaderReferer req of
                Nothing      -> return ()
                Just referer -> do
                    (mauth,refPath) <- parseUrl referer
                    when (isNothing mauth
                       || requestHeaderHost req == mauth) $ do
                        when (path /= refPath) $ do -- just in case
                            mpp <- makePushPromise refPath path file
                            case mpp of
                                Nothing -> return ()
                                Just pp -> reaperAdd reaper (refPath,pp)
            ps -> do
                let !h2d = defaultHTTP2Data { http2dataPushPromise = ps}
                setHTTP2Data req (Just h2d)
        sendResponse res
    push _ res = sendResponse res


-- | Learn if the file to be pushed is CSS (.css) or JavaScript (.js) file
--   AND the Referer: ends with \"/\" or \".html\" or \".htm\".
defaultMakePushPromise :: MakePushPromise
defaultMakePushPromise refPath path file
  | isHTML refPath = case getCT path of
      Nothing -> return Nothing
      Just ct -> do
          let pp = defaultPushPromise {
                       promisedPath = path
                     , promisedFile = file
                     , promisedResponseHeaders = [("content-type", ct)
                                                 ,("x-http2-push", refPath)]
                     }
          return $ Just pp
  | otherwise = return Nothing

getCT :: URLPath -> Maybe ByteString
getCT p
  | ".js"  `BS.isSuffixOf` p = Just "application/javascript"
  | ".css" `BS.isSuffixOf` p = Just "text/css"
  | otherwise                = Nothing

isHTML :: URLPath -> Bool
isHTML p = ("/" `BS.isSuffixOf` p)
        || (".html" `BS.isSuffixOf` p)
        || (".htm" `BS.isSuffixOf` p)

-- |
--
-- >>> parseUrl ""
-- (Nothing,"")
-- >>> parseUrl "/"
-- (Nothing,"/")
-- >>> parseUrl "ht"
-- (Nothing,"")
-- >>> parseUrl "http://example.com/foo/bar/"
-- (Just "example.com","/foo/bar/")
-- >>> parseUrl "https://www.example.com/path/to/dir/"
-- (Just "www.example.com","/path/to/dir/")
-- >>> parseUrl "http://www.example.com:8080/path/to/dir/"
-- (Just "www.example.com:8080","/path/to/dir/")
-- >>> parseUrl "//www.example.com:8080/path/to/dir/"
-- (Just "www.example.com:8080","/path/to/dir/")
-- >>> parseUrl "/path/to/dir/"
-- (Nothing,"/path/to/dir/")

parseUrl :: ByteString -> IO (Maybe ByteString, URLPath)
parseUrl bs@(PS fptr0 off len)
  | len == 0 = return (Nothing, "")
  | len == 1 = return (Nothing, bs)
  | otherwise = withForeignPtr fptr0 $ \ptr0 -> do
      let begptr = ptr0 `plusPtr` off
          limptr = begptr `plusPtr` len
      parseUrl' fptr0 ptr0 begptr limptr len

parseUrl' :: ForeignPtr Word8 -> Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> Int
          -> IO (Maybe ByteString, URLPath)
parseUrl' fptr0 ptr0 begptr limptr len0 = do
      w0 <- peek begptr
      if w0 == _slash then do
          w1 <- peek $ begptr `plusPtr` 1
          if w1 == _slash  then
              doubleSlashed begptr len0
            else
              slashed begptr len0 Nothing
        else do
          colonptr <- memchr begptr _colon $ fromIntegral len0
          if colonptr == nullPtr then
              return (Nothing, "")
            else do
              let !authptr = colonptr `plusPtr` 1
              doubleSlashed authptr (limptr `minusPtr` authptr)
  where
    -- // / ?
    doubleSlashed :: Ptr Word8 -> Int -> IO (Maybe ByteString, URLPath)
    doubleSlashed ptr len
      | len < 2  = return (Nothing, "")
      | otherwise = do
          let ptr1 = ptr `plusPtr` 2
          pathptr <- memchr ptr1 _slash $ fromIntegral len
          if pathptr == nullPtr then
              return (Nothing, "")
            else do
              let !auth = bs ptr0 ptr1 pathptr
              slashed pathptr (limptr `minusPtr` pathptr) (Just auth)

    -- / ?
    slashed :: Ptr Word8 -> Int -> Maybe ByteString -> IO (Maybe ByteString, URLPath)
    slashed ptr len mauth = do
        questionptr <- memchr ptr _question $ fromIntegral len
        if questionptr == nullPtr then do
            let !path = bs ptr0 ptr limptr
            return (mauth, path)
          else do
            let !path = bs ptr0 ptr questionptr
            return (mauth, path)
    bs p0 p1 p2 = path
      where
        !off = p1 `minusPtr` p0
        !siz = p2 `minusPtr` p1
        !path = PS fptr0 off siz
