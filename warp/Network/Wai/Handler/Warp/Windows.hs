{-# LANGUAGE CPP #-}
module Network.Wai.Handler.Warp.Windows
  ( windowsThreadBlockHack
  ) where

#if WINDOWS
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Control.Exception

import Network.Wai.Handler.Warp.Imports

-- | Allow main socket listening thread to be interrupted on Windows platform
--
-- @since 3.2.17
windowsThreadBlockHack :: IO a -> IO a
windowsThreadBlockHack act = do
    var <- newEmptyMVar :: IO (MVar (Either Control.Exception.SomeException a))
    -- Catch and rethrow even async exceptions, so don't bother with UnliftIO
    void . forkIO $ Control.Exception.try act >>= putMVar var
    res <- takeMVar var
    case res of
      Left  e -> Control.Exception.throwIO e
      Right r -> return r
#else
windowsThreadBlockHack :: IO a -> IO a
windowsThreadBlockHack = id
#endif
