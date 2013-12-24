{-# LANGUAGE CPP #-}
-- | A module for containing some CPPed code, due to:
--
-- https://github.com/yesodweb/wai/issues/192
module Network.Wai.Middleware.RequestLogger.Internal
    ( module Network.Wai.Middleware.RequestLogger.Internal
    , toByteString
    ) where

import Data.ByteString (ByteString)
import Network.Wai.Logger (clockDateCacher)

#if MIN_VERSION_bytestring(0, 10, 2)
import Data.ByteString.Builder (toLazyByteString, Builder)
import qualified Data.ByteString.Lazy as L

toByteString :: Builder -> ByteString
toByteString = L.toStrict . toLazyByteString

#else
import Blaze.ByteString.Builder (toByteString)
#endif

getDateGetter :: IO (IO ByteString)
getDateGetter = fmap fst clockDateCacher
