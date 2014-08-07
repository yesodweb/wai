{-# LANGUAGE CPP #-}
module Control.AutoUpdate.Util
    ( atomicModifyIORef'
    ) where

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

#if MIN_VERSION_base(4,6,0)
import           Data.IORef         (atomicModifyIORef')
#else
import           Data.IORef         (IORef, atomicModifyIORef)
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
