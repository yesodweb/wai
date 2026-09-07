{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.Wai.Middleware.Push.Referer.Manager (
    MakePushPromise,
    defaultMakePushPromise,
    Settings (..),
    defaultSettings,
    Manager,
    URLPath,
    getManager,
    Network.Wai.Middleware.Push.Referer.Manager.lookup,
    Network.Wai.Middleware.Push.Referer.Manager.insert,
) where

import Control.Monad (unless)
import Data.IORef
import Network.Wai.Handler.Warp hiding (Settings, defaultSettings)
import System.IO.Unsafe (unsafePerformIO)

import qualified Network.Wai.Middleware.Push.Referer.LRU as LRU
import Network.Wai.Middleware.Push.Referer.Types

newtype Manager = Manager (IORef (LRU.Cache URLPath PushPromise))

getManager :: Settings -> IO Manager
getManager Settings{..} = do
    isInitialized <- atomicModifyIORef' lruInitialized $ \x -> (True, x)
    unless isInitialized $ do
        let cache = LRU.empty keyLimit valueLimit
            Manager ref = cacheManager
        writeIORef ref cache
    return cacheManager

lruInitialized :: IORef Bool
lruInitialized = unsafePerformIO $ newIORef False
{-# NOINLINE lruInitialized #-}

cacheManager :: Manager
cacheManager = Manager $ unsafePerformIO $ newIORef $ LRU.empty 0 0
{-# NOINLINE cacheManager #-}

lookup :: URLPath -> Manager -> IO [PushPromise]
lookup k (Manager ref) = atomicModifyIORef' ref $ LRU.lookup k

insert :: URLPath -> PushPromise -> Manager -> IO ()
insert k v (Manager ref) = atomicModifyIORef' ref $ \c -> (LRU.insert k v c, ())
