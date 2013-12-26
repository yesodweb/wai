{-# LANGUAGE CPP #-}
-- | A module for containing some CPPed code, due to:
--
-- https://github.com/yesodweb/wai/issues/192
module Network.Wai.Middleware.RequestLogger.Internal
    ( module Network.Wai.Middleware.RequestLogger.Internal
    , logToByteString
    ) where

import Data.ByteString (ByteString)
import Network.Wai.Logger (clockDateCacher)

#if MIN_VERSION_fast_logger(2, 1, 0)
import System.Log.FastLogger (LogStr, fromLogStr)

logToByteString :: LogStr -> ByteString
logToByteString = fromLogStr

#else

import System.Log.FastLogger (LogStr, logStrBuilder)

#if MIN_VERSION_bytestring(0, 10, 2)
import Data.ByteString.Builder (toLazyByteString, Builder)
import qualified Data.ByteString.Lazy as L

toByteString :: Builder -> ByteString
toByteString = L.toStrict . toLazyByteString

#else
import Blaze.ByteString.Builder (toByteString)
#endif

logToByteString :: LogStr -> ByteString
logToByteString = toByteString . logStrBuilder

#endif

getDateGetter :: IO (IO ByteString)
getDateGetter = fmap fst clockDateCacher
