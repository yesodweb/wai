{-# LANGUAGE CPP #-}
module Network.Wai.Handler.Warp.Windows
  ( windowsThreadBlockHack
  ) where

#if WINDOWS
import Control.Exception
import Control.Concurrent.MVar
import Control.Concurrent

import Network.Wai.Handler.Warp.Imports

-- | Allow main socket listening thread to be interrupted on Windows platform
--
-- @since 3.2.17
windowsThreadBlockHack :: IO a -> IO a
windowsThreadBlockHack act = do
    var <- newEmptyMVar :: IO (MVar (Either SomeException a))
    void . forkIO $ try act >>= putMVar var
    res <- takeMVar var
    case res of
      Left  e -> throwIO e
      Right r -> return r
#else
windowsThreadBlockHack :: IO a -> IO a
windowsThreadBlockHack = id
#endif
