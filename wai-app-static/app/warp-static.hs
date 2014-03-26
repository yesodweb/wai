{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main (main) where

import WaiAppStatic.CmdLine (runCommandLine)

main :: IO ()
main = runCommandLine (const id)
