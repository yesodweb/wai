{-|
Module      : Devel
Description : An entry point for GHC to compile yesod-devel.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}
module Devel (buildAndRun) where

-- Related to config
import IdeSession (sessionConfigFromEnv)
import System.Environment (lookupEnv, setEnv)

-- Internal functions.
import Devel.Config (setConfig)
import Devel.ReverseProxy (cyclePorts)
import Devel.Build (build)

-- | Build and run our haskell application.
buildAndRun :: FilePath -> String ->  Bool -> IO ()
buildAndRun buildFile runFunction isReverseProxy = do

  -- set Environment variables: GHC_PACKAGE_PATH and PATH
  -- Needed for ide-backend to provide sessionConfig
  _ <- setConfig


  -- Let ide-backend set session config now because ide-backend can't set it during rebuilds.
  -- We then pass sessionConfig around between rebuilds
  -- You must restart wai-devel in the terminal for it to get a new session config
  sessionConfig <- sessionConfigFromEnv

  -- If port isn't set we assume port 3000
  maybePort <- lookupEnv "PORT"
  let fromProxyPort = case maybePort of
                       Just port -> read port :: Int
                       _ -> 3000

  -- Look for and give us a port to reverse proxy to
  -- i.e a destination port.
  toProxyPort <- cyclePorts fromProxyPort

  -- Set PORT depending on whether reverse proxying is disallowed or not.
  -- If reverse proxying is set to disabled i.e False
  -- Then PORT variable is set to the source port i.e fromProxyPort
  -- By default reverse proxying is True.
  if isReverseProxy
     then setEnv "PORT" (show toProxyPort)
     else setEnv "PORT" (show fromProxyPort)

  -- We call the build function only the first time we want to build our application.
  build
    buildFile -- The target file, should contain the function we wish to call.
    runFunction -- The function we want to call.
    isReverseProxy -- Use reverse proxying? Default to True. 
    sessionConfig -- We need this when compiling.
    (fromProxyPort, toProxyPort) -- The port we reverse proxy to and from.
    Nothing -- Maybe IdeSession
    False  -- To rebuild or not to.
