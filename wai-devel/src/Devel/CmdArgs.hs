{-|
Module      : Devel.Args
Description : For handling command line arguments.
Copyright   : (c)
License     : GPL-3
Maintainer  : njagi@urbanslug.com
Stability   : experimental
Portability : POSIX
-}
module Devel.CmdArgs
(  cmdArgs
,  CmdArgs (..)
) where

import Options.Applicative

-- | Command line arguments for yesod devel.
-- All arguments are optional.
data CmdArgs = CmdArgs
  { buildFile :: FilePath
  , runFunction :: String
  , isReverseProxy :: Bool -- By default reverse proxy should be True 
  } deriving (Show, Eq)

cmdArgs :: Parser CmdArgs
cmdArgs = CmdArgs
        <$>  strOption
             (long "path"
               <> short 'p'
               <> value "Application.hs"
               <> metavar "FILEPATH"
               <> help "The file with the function you want to run. Default is `Application.hs`.")
        <*>  strOption
               (long "function"
                 <> short 'f'
                 <> value "develMain"
                 <> metavar "FUNCTION"
                 <> help "The function you want run. Default is `develMain`.")
        <*> flag True False
              (long "no-reverse-proxy"
                <> short 'r'
                <> help "use `-r` to disable reverse proxying." )
        
