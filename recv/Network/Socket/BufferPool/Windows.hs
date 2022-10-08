{-# LANGUAGE CPP #-}
module Network.Socket.BufferPool.Windows
  ( windowsThreadBlockHack
  ) where

#ifdef mingw32_HOST_OS
import Control.Concurrent.MVar
import Control.Concurrent
import qualified Control.Exception
import Control.Monad

-- | Allow main socket listening thread to be interrupted on Windows platform
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
