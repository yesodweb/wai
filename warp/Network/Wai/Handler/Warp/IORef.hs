{-# LANGUAGE CPP #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

module Network.Wai.Handler.Warp.IORef (
    module Data.IORef
#if !MIN_VERSION_base(4,6,0)
  , atomicModifyIORef'
#endif
  ) where

import Data.IORef

#if !MIN_VERSION_base(4,6,0)
-- | Strict version of 'atomicModifyIORef'.  This forces both the value stored
-- in the 'IORef' as well as the value returned.
atomicModifyIORef' :: IORef a -> (a -> (a,b)) -> IO b
atomicModifyIORef' ref f = do
    c <- atomicModifyIORef ref
            (\x -> let (a, b) = f x    -- Lazy application of "f"
                    in (a, a `seq` b)) -- Lazy application of "seq"
    -- The following forces "a `seq` b", so it also forces "f x".
    c `seq` return c
#endif
