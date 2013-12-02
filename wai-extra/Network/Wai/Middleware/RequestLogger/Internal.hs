{-# LANGUAGE CPP #-}
-- | A module for containing some CPPed code, due to:
--
-- https://github.com/yesodweb/wai/issues/192
module Network.Wai.Middleware.RequestLogger.Internal where

import Data.ByteString (ByteString)
import Network.Wai.Logger (clockDateCacher)

getDateGetter :: IO (IO ByteString)
getDateGetter = fmap fst clockDateCacher
