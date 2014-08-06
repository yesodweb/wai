{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Date (
    withDateCache
  , getDate
  , DateCache
  , GMTDate
  ) where

import Control.Applicative
import Control.AutoUpdate (defaultUpdateSettings, updateAction, mkAutoUpdate)
import Data.ByteString.Char8

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
type DateCache = IO GMTDate

-- | Creating 'DateCache' and executing the action.
withDateCache :: (DateCache -> IO a) -> IO a
withDateCache action = initialize >>= action

initialize :: IO DateCache
initialize = mkAutoUpdate defaultUpdateSettings { updateAction = getCurrentGMTDate }

-- | Getting 'GMTDate' based on 'DateCache'.
getDate :: DateCache -> IO GMTDate
getDate = id

getCurrentGMTDate :: IO GMTDate
#ifdef WINDOWS
getCurrentGMTDate = formatDate <$> getCurrentTime
  where
    formatDate = pack . formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT"
#else
getCurrentGMTDate = formatHTTPDate . epochTimeToHTTPDate <$> epochTime
#endif
