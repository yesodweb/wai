{-# LANGUAGE CPP #-}
-- | A module for containing some CPPed code, due to:
--
-- https://github.com/yesodweb/wai/issues/192
module Network.Wai.Middleware.RequestLogger.Internal
    ( module Network.Wai.Middleware.RequestLogger.Internal
    ) where

import Data.ByteString (ByteString)
import Network.Wai.Logger (clockDateCacher)
#if !MIN_VERSION_wai_logger(2, 2, 0)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad (forever)
#endif
import System.Log.FastLogger (LogStr, fromLogStr)

logToByteString :: LogStr -> ByteString
logToByteString = fromLogStr

getDateGetter :: IO () -- ^ flusher
              -> IO (IO ByteString)
#if !MIN_VERSION_wai_logger(2, 2, 0)
getDateGetter flusher = do
    (getter, updater) <- clockDateCacher
    _ <- forkIO $ forever $ do
        threadDelay 1000000
        updater
        flusher
#else
getDateGetter _ = do
    (getter, _) <- clockDateCacher
#endif
    return getter
