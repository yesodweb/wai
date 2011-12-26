module Network.Wai.Middleware.Debug
    {-# DEPRECATED "functionality has been moved to the better named Network.Wai.Middleware.RequestLogger. Network.Wai.Middleware.Debug will be removed." #-}
    ( debug
    , debugHandle
    ) where

import Network.Wai.Middleware.RequestLogger (logStdoutDevLT, logHandleDevLT)
import Network.Wai (Middleware)
import qualified Data.Text.Lazy as LT

-- | Deprecated. Use module Network.Wai.Middleware.RequestLogger
debug :: Middleware
debug = logStdoutDevLT

-- | Deprecated. Use module Network.Wai.Middleware.RequestLogger
debugHandle :: (LT.Text -> IO ()) -> Middleware
debugHandle = logHandleDevLT
