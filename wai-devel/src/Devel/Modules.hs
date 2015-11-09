{-|
Module      : Devel.Modules
Description : For handling modules and Filepath matters.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Resolve filepaths from module names.
-}

{-# LANGUAGE PackageImports #-}

module Devel.Modules (getCompiledFiles) where

import IdeSession
import System.FilePath (pathSeparator)
import Data.Text (unpack)
import Data.String.Utils
import System.Directory (getCurrentDirectory)

-- | Paths without extensions.
-- Uncomment for absolute paths.
getCompiledFiles :: IdeSession -> IO [FilePath]
getCompiledFiles session = do

  moduleList <- getLoadedModules session
  dir <- getCurrentDirectory

  let toFilePath :: ModuleName -> FilePath -- A.B.C = A/B/C
      toFilePath moduleName' = replace "." [pathSeparator] $ unpack moduleName'
      absPaths = map (((dir ++ "/") ++) . toFilePath) moduleList

  -- We use absolute paths because when we match them against dir contents we use absolute paths.
  -- or the result of Devel.Paths.getRecursiveContents
  return absPaths
