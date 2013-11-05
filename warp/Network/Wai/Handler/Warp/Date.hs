{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Date (
    withDateCache
  , getDate
  , DateCache
  , GMTDate
  ) where

import Control.Applicative
import Control.Concurrent
import Control.Exception
import Control.Monad
import Data.ByteString.Char8
import Data.IORef

#if WINDOWS
import Data.Time
import System.Locale
#else
import Network.HTTP.Date
import System.Posix (epochTime)
#endif

-- | The type of the Date header value.
type GMTDate = ByteString

-- | The type of the cache of the Date header value.
data DateCache = DateCache (IORef GMTDate)

-- | Creating 'DateCache' and executing the action.
withDateCache :: (DateCache -> IO a) -> IO a
withDateCache action = bracket initialize
                               (\(t,_) -> killThread t)
                               (\(_,dc) -> action dc)

initialize :: IO (ThreadId, DateCache)
initialize = do
    dc <- DateCache <$> (getCurrentGMTDate >>= newIORef)
    t <- forkIO $ forever $ do
        threadDelay 1000000
        update dc
    return (t, dc)

-- | Getting 'GMTDate' based on 'DateCache'.
getDate :: DateCache -> IO GMTDate
getDate (DateCache ref) = readIORef ref

update :: DateCache -> IO ()
update (DateCache ref) = getCurrentGMTDate >>= writeIORef ref

getCurrentGMTDate :: IO GMTDate
#ifdef WINDOWS
getCurrentGMTDate = formatDate <$> getCurrentTime
  where
    formatDate = pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"
#else
getCurrentGMTDate = formatHTTPDate . epochTimeToHTTPDate <$> epochTime
#endif
