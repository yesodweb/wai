{-|
Module      : Devel.Compile
Description : For building and running your WAI application.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Devel.Build 
( build
) where


import IdeSession
import qualified Data.ByteString.Char8 as S8
import Data.Text (unpack)

import GHC.Conc (newTVarIO)
import Control.Concurrent (forkIO, killThread, ThreadId)

-- Rebuild
import Control.Monad (unless)
# if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mempty)
#endif

import Devel.Compile
import Devel.ReverseProxy (startReverseProxy)
import Devel.Watch


-- | Compiles and calls run on your WAI application.
build :: FilePath -> String ->  Bool -> SessionConfig -> (Int, Int) -> Maybe IdeSession -> Bool -> IO ()
build buildFile runFunction isReverseProxy sessionConfig (fromProxyPort, toProxyPort) mSession isRebuild = do

  (initialSession, extensionList, includeTargets) <- initCompile sessionConfig mSession

  -- Do this if isRebuild is False.
  unless isRebuild $
    if isReverseProxy then
      do _ <- forkIO $ startReverseProxy (fromProxyPort, toProxyPort)
         putStrLn $ "Starting devel application at http://localhost:" ++
                    show fromProxyPort
    else
      putStrLn $ "Starting app without reverse proxying at http://localhost:" ++
                 show fromProxyPort


  (updatedSession, update) <- 
    if isRebuild
       then return (initialSession, mempty)
       else compile initialSession buildFile extensionList includeTargets

  eitherSession <- finishCompile (updatedSession, update)

  case eitherSession of
    Left _ -> do
      -- Listen for changes in the current working directory.
      isDirty <- newTVarIO False

      _ <- forkIO $ watch isDirty includeTargets

      -- Block until relevant change is made then carry on with program execution.
      _ <- checkForChange isDirty

      -- Stop the current app.
      putStrLn "\n\nRebuilding...\n\n"
      
      _ <- shutdownSession updatedSession

      -- Coming to fix.
      build buildFile runFunction False sessionConfig (fromProxyPort, toProxyPort) Nothing False

    Right session -> do
      -- run the session
      (runActionsRunResult, threadId) <- run session buildFile runFunction

      -- Start watching for file changes.
      isDirty <- newTVarIO False

      -- Watch for changes in the current working directory.
      watchId <- forkIO $ watch isDirty includeTargets

      -- Block until relevant change is made then carry on with program execution.
      _ <- checkForChange isDirty

      killThread watchId
      
      -- Stop the current app.
      _ <- stopApp runActionsRunResult threadId
      putStrLn "\n\nRebuilding...\n\n"
      build buildFile runFunction isReverseProxy sessionConfig (fromProxyPort, toProxyPort) (Just session) True


run :: IdeSession -> FilePath -> String -> IO (RunActions RunResult, ThreadId)
run session buildFile runFunction = do
  -- Get the module name from the file path
  mapFunction <- getFileMap session
  buildModule <- case mapFunction buildFile of
                   Nothing -> fail $ "The file's module name for: " ++ show buildFile ++" couldn't be found"
                   Just moduleId -> return $ unpack $ moduleName moduleId

  -- Run the given ide-backend session.
  runActionsRunResult <- runStmt session buildModule runFunction
  threadId <- forkIO $ loop runActionsRunResult

  return (runActionsRunResult, threadId)
  

-- | Stop the currently running WAI application.
stopApp :: RunActions RunResult -> ThreadId -> IO ()
stopApp runResult threadId = do
  interrupt runResult
  killThread threadId


-- | Run for as long as we need to.
loop :: RunActions RunResult -> IO ()
loop res = do
  runAction <- runWait res
  case runAction of
    Left bs -> S8.putStr bs >> loop res
    Right result -> putStrLn $ "Run result: " ++ show result

