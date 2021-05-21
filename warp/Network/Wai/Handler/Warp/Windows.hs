{-# LANGUAGE CPP #-}
module Network.Wai.Handler.Warp.Windows
  ( windowsThreadBlockHack
  ) where

#if WINDOWS
import qualified UnliftIO
import Control.Concurrent.MVar
import Control.Concurrent

import Network.Wai.Handler.Warp.Imports

-- | Allow main socket listening thread to be interrupted on Windows platform
--
-- @since 3.2.17
windowsThreadBlockHack :: IO a -> IO a
windowsThreadBlockHack act = do
    var <- newEmptyMVar :: IO (MVar (Either UnliftIO.SomeException a))
    void . forkIO $ UnliftIO.tryAny act >>= putMVar var
    res <- takeMVar var
    case res of
      Left  e -> throwIO e
      Right r -> return r
#else
windowsThreadBlockHack :: IO a -> IO a
windowsThreadBlockHack = id
#endif
