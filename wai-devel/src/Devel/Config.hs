{-|
Module      : Devel.Config
Description : To have wai-devel depend on it's environment a lot less.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX

Currently we'll query for this information from the stack binary.

Ideal case it to use the stack library to figure out depends and stuff.
Depending on the stack library causes a breakage in the dependecy tree.
As of now I can't find a single function (or set of functions) that fetches this information from stack.
Closest thing is in the stack Main module.
        
Will be rewritten to depend on the stack library.
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Devel.Config 
( setConfig
, getPath
, getPkgDb
) where

import System.Process (readProcessWithExitCode)
import System.Environment (unsetEnv, setEnv, lookupEnv)
import Control.Exception (catch)

import System.FilePath.Posix (pathSeparator)
import System.Directory (getCurrentDirectory, getHomeDirectory, doesDirectoryExist)
import System.Info (arch, compilerName, os)

import Devel.Types

setConfig :: IO ()
setConfig = do
  (path, pkgDb) <- getConfig
  _ <- unsetEnv "PATH"
  _ <- unsetEnv "GHC_PACKAGE_PATH"
  _ <- setEnv "PATH" path
  setEnv "GHC_PACKAGE_PATH" pkgDb
  

getConfig :: IO Config
getConfig = do
  -- If stack isn't installed use cabal-install.
  catch getStackConfig getCabalConfig
  where 
    getStackConfig :: IO Config
    getStackConfig = do 
      (_, stdout, _) <- readProcessWithExitCode "stack" ["path"] ""
      parseConfig stdout


    parseConfig :: String -> IO Config
    parseConfig stdout = do
      let outputList = lines stdout
          tupleList = map (span (/=':') ) outputList
          path = concatMap getPath tupleList
          pkgDb = concatMap getPkgDb tupleList
      return (path, pkgDb)

getCabalConfig :: IOError -> IO Config
getCabalConfig _ = do
  mPath <- lookupEnv "PATH"
  mPkgDb <- lookupEnv "GHC_PACKAGE_PATH"
  
  ghcVersion <- catch getGhcVersion getGhcVersionError
  
  cwd <- getCurrentDirectory
  homeDir <- getHomeDirectory
  isSandBoxed <- doesDirectoryExist ".cabal-sandbox"
  
  let compilerVersion' = last $ words ghcVersion
      forSandboxPath = arch ++ "-" ++ os ++ "-" ++ compilerName ++ "-" ++ compilerVersion'
      forUserPath    = arch ++ "-" ++ os ++ "-" ++ compilerVersion'
      forGlobalPath  = compilerName ++ "-" ++ compilerVersion'


      sandboxPkgDb = if isSandBoxed
                        then cwd ++ (pathSeparator : ".cabal-sandbox") 
                             ++ (pathSeparator : forSandboxPath) ++ "-packages.conf.d:"
                        else "" -- We're not using a sandbox.

      userPkgDb = homeDir ++ (pathSeparator: ".ghc") ++ (pathSeparator:forUserPath) 
                  ++ (pathSeparator:"package.conf.d:")
      globalPkgDb = "/usr/lib/" ++ forGlobalPath ++"/package.conf.d:"

      pkgDb' = sandboxPkgDb ++ userPkgDb ++ globalPkgDb

      path = case mPath of
                   Just p -> p
                   Nothing -> fail "The environment variable PATH isn't set."
      pkgDb = case mPkgDb of
                   Just db -> db
                   Nothing -> pkgDb'

  return (path, pkgDb)

getGhcVersion :: IO String
getGhcVersion = do
  (_, stdout, _) <- readProcessWithExitCode "ghc" ["--version"] ""
  return stdout

-- Give proper fail information.
getGhcVersionError :: IOError -> IO String
getGhcVersionError _ = do
  fail $ "Is GHC not in your PATH? "
         ++ "Because wai-devel can't get the version number from: "
         ++ "`ghc --version`"

getPath :: (String, String) -> String
getPath (key,value)
        | key == "bin-path" = dropWhile (==':') $ filter (/=' ') value
        | otherwise = ""

getPkgDb :: (String, String) -> String
getPkgDb (key,value)
          | key == "ghc-package-path" = dropWhile (==':') $ filter (/=' ') value
          | otherwise = ""
