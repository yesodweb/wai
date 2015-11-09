{-|
Module      : Devel.Paths
Description : For filepath related matters.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Uses the GHC package to parse .hi files.
Will hopefully be moved upstream to ide-backend.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module Devel.Paths 
( getFilesToWatch
, getThFiles
, getCabalFile
, getRecursiveContents
-- , getRecursiveContentsRelative
) where

-- Qualified imports
import qualified Data.ByteString.Char8 as C8

import "Glob" System.FilePath.Glob (globDir, compile, Pattern, glob)
import System.FilePath.Posix (replaceExtension)
import System.Directory (doesFileExist, removeFile, getDirectoryContents, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forM,filterM)
import Data.List ((\\))


getThFiles :: [FilePath] -> IO [FilePath]
getThFiles targetList = do 
  let dumpFiles' = map (`replaceExtension` "dump-hi") targetList
  dumpFiles <- filterM doesFileExist dumpFiles'
  thFiles' <- mapM parseHi dumpFiles
  let thFiles  = map (takeWhile (/='\"') . dropWhile (=='\"') . dropWhile (/='\"')) $ concat thFiles'

  _ <- mapM removeFile dumpFiles
  return thFiles

getFilesToWatch :: [FilePath]  -> IO [FilePath]
getFilesToWatch targetList = do
  thFiles <- getThFiles targetList
  return $ thFiles ++ targetList



parseHi :: FilePath -> IO [FilePath]
parseHi path = do
  dumpHI <- liftIO $ fmap C8.lines (C8.readFile path)
  let thDeps' =
          -- The dependent file path is surrounded by quotes but is not escaped.
          -- It can be an absolute or relative path.
          filter ("addDependentFile \"" `C8.isPrefixOf`) dumpHI
  return $ map C8.unpack thDeps'



getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents topdir = do
  names <- getDirectoryContents topdir

  -- We want to take these files out.
  let patterns :: [Pattern]
      patterns = [ (compile "*.*~")
                 , (compile "*.hi")
                 , (compile "*.dump-hi")
                 , (compile "*.o")
                 , (compile "*.dyn_o")
                 , (compile "*.dyn_hi")
                 , (compile "*.so")
                 , (compile "*.conf")
                 , (compile "*.h")
                 , (compile "*.a")
                 , (compile "*.inplace")
                 , (compile "*.cache")
                 , (compile "*.*.el")
                 , (compile ".*")
                 ]
  (x, _) <- globDir patterns topdir

  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \name -> do
   let path = makePathRelative topdir </> name
   isDirectory <- doesDirectoryExist path
   if isDirectory
     then getRecursiveContents path
     else return $ [path] \\ concat x
  return (concat paths)
  where makePathRelative :: FilePath -> FilePath
        makePathRelative topDir 
          | topDir == "." = ""
          | otherwise = topDir


getCabalFile :: IO FilePath
getCabalFile = do
  list <- glob "*cabal"
  case list of
    [] -> fail "No cabal file."
    (cabalFile:_) -> return cabalFile
