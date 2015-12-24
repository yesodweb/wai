{-# LANGUAGE CPP #-}

module Network.Wai.Handler.Warp.Date (
    withDateCache
  , getDate
  , DateCache
  , GMTDate
  ) where

#if __GLASGOW_HASKELL__ < 709
import Control.Applicative
#endif
import Control.AutoUpdate (defaultUpdateSettings, updateAction, mkAutoUpdate)
import Data.ByteString.Char8
import Network.HTTP.Date

#if WINDOWS
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Foreign.C.Types (CTime(..))
#else
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
initialize = mkAutoUpdate defaultUpdateSettings {
                            updateAction = formatHTTPDate <$> getCurrentHTTPDate
                          }

-- | Getting current 'GMTDate' based on 'DateCache'.
getDate :: DateCache -> IO GMTDate
getDate = id

#ifdef WINDOWS
uToH :: UTCTime -> HTTPDate
uToH = epochTimeToHTTPDate . CTime . truncate . utcTimeToPOSIXSeconds

getCurrentHTTPDate :: IO HTTPDate
getCurrentHTTPDate =  uToH <$> getCurrentTime
#else
getCurrentHTTPDate :: IO HTTPDate
getCurrentHTTPDate = epochTimeToHTTPDate <$> epochTime
#endif
