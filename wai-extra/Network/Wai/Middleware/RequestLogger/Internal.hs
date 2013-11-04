{-# LANGUAGE CPP #-}
-- | A module for containing some CPPed code, due to:
--
-- https://github.com/yesodweb/wai/issues/192
module Network.Wai.Middleware.RequestLogger.Internal where

import Data.ByteString (ByteString)
import System.Log.FastLogger
#if MIN_VERSION_fast_logger(0,3,0)
import System.Date.Cache (ondemandDateCacher)
#else
import System.Log.FastLogger.Date (getDate, dateInit, ZonedDate)
#endif

getDateGetter :: IO (IO ByteString)
#if MIN_VERSION_fast_logger(0, 3, 0)
getDateGetter = fmap fst $ ondemandDateCacher zonedDateCacheConf
#else
getDateGetter = fmap getDate dateInit
#endif
