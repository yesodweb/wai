{-# LANGUAGE CPP #-}
module Network.Wai.Handler.Warp.Environment (lookupEnv) where

import System.Environment

#if !MIN_VERSION_base(4,6,0)
lookupEnv :: String -> IO (Maybe String)
lookupEnv k = lookup k `fmap` getEnvironment
#endif
