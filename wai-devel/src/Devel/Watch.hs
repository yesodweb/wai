{-|
Module      : Devel.Watch
Description : Watch for changes in the current working direcory.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Actually checks only for modified files.
Added or removed files don't trigger new builds.
-}
{-# LANGUAGE OverloadedStrings, CPP #-}
module Devel.Watch where

-- import IdeSession

import Control.Monad.STM
import Control.Concurrent.STM.TVar

import System.FSNotify
import Control.Monad      (forever)
import Control.Concurrent (threadDelay)

-- import Devel.Types
-- import Devel.Paths (getFilesToWatch)

# if __GLASGOW_HASKELL__ < 710
import Data.Text (unpack)
import Filesystem.Path.CurrentOS (toText)
-- import qualified Filesystem.Path as FSP
#endif

import System.Directory (getCurrentDirectory)
import System.FilePath (pathSeparator)

import Devel.Paths

watch :: TVar Bool -> [FilePath] -> IO ()
watch isDirty includeTargets = do
  -- Get files to watch.
  files <- getFilesToWatch includeTargets
  -- Making paths to watch a list of absolute paths.
  dir <- getCurrentDirectory
  let pathsToWatch = map (\fp -> dir ++ (pathSeparator: fp)) files

  -- Actual file watching.
  manager <- startManagerConf defaultConfig
  _ <- watchTree manager "." (const True)
         -- Last argument to watchTree.
# if __GLASGOW_HASKELL__ >= 710
         (\event -> do
            let getPath :: Event -> FilePath
                getPath (Added fp _)    = fp
                getPath (Modified fp _) = fp
                getPath (Removed fp _)  = fp
                
                isModified = getPath event `elem` pathsToWatch
                
            atomically $ writeTVar isDirty isModified)
            
#else
         (\event -> do 
            pathMod' <- case toText $ eventPath event of
                            Right text -> return $ unpack text -- Gives an abs path
                            Left text -> fail $ unpack text
                            
            let isModified = pathMod' `elem` pathsToWatch
            
            atomically $ writeTVar isDirty isModified)
#endif

  _ <- forever $ threadDelay maxBound
  stopManager manager

checkForChange :: TVar Bool -> IO ()
checkForChange isDirty =
  atomically $ do readTVar isDirty >>= check
                  writeTVar isDirty False
