{-# LANGUAGE CPP #-}
module Network.Wai.Handler.DevelServer.Compat (
  TimeStamp
, getTimeStamp
) where

import System.Directory (getModificationTime)

#if MIN_VERSION_directory(1,2,0)
import Data.Time.Clock (UTCTime)
newtype TimeStamp = TimeStamp UTCTime
#else
import System.Time (ClockTime)
newtype TimeStamp = TimeStamp ClockTime
#endif
  deriving Eq

getTimeStamp :: FilePath -> IO TimeStamp
getTimeStamp = fmap TimeStamp . getModificationTime
