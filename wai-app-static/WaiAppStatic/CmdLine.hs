{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}

-- | Command line version of wai-app-static, used for the warp-static server.
module WaiAppStatic.CmdLine (
    runCommandLine,
    Args (..),
) where

import Control.Arrow (second, (***))
import Control.Monad (unless)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import Data.String (fromString)
import Data.Text (pack)
import Network.Mime (defaultMimeMap, defaultMimeType, mimeByExt)
import Network.Wai (Middleware)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp (
    defaultSettings,
    runSettings,
    setHost,
    setPort,
 )
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.RequestLogger (logStdout)
import Options.Applicative
import System.Directory (canonicalizePath)
import Text.Printf (printf)
import WaiAppStatic.Types (
    fileName,
    fromPiece,
    ssGetMimeType,
    ssIndices,
    toPiece,
 )
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid ((<>))
#endif

data Args = Args
    { docroot :: FilePath
    , index :: [FilePath]
    , port :: Int
    , noindex :: Bool
    , quiet :: Bool
    , verbose :: Bool
    , mime :: [(String, String)]
    , host :: String
    }

#if MIN_VERSION_optparse_applicative(0, 10, 0)
option' :: Mod OptionFields Int -> Parser Int
option' = option auto
#else
option' = option
#endif

args :: Parser Args
args =
    Args
        <$> strOption
            ( long "docroot"
                <> short 'd'
                <> metavar "DOCROOT"
                <> value "."
                <> help "directory containing files to serve"
            )
        <*> ( defIndex
                <$> many
                    ( strOption
                        ( long "index"
                            <> short 'i'
                            <> metavar "INDEX"
                            <> help "index files to serve when a directory is required"
                        )
                    )
            )
        <*> option'
            ( long "port"
                <> short 'p'
                <> metavar "PORT"
                <> value 3000
            )
        <*> switch
            ( long "noindex"
                <> short 'n'
            )
        <*> switch
            ( long "quiet"
                <> short 'q'
            )
        <*> switch
            ( long "verbose"
                <> short 'v'
            )
        <*> many
            ( toPair
                <$> strOption
                    ( long "mime"
                        <> short 'm'
                        <> metavar "MIME"
                        <> help "extra file extension/mime type mappings"
                    )
            )
        <*> strOption
            ( long "host"
                <> short 'h'
                <> metavar "HOST"
                <> value "*"
                <> help "interface to bind to, special values: *, *4, *6"
            )
  where
    toPair = second (drop 1) . break (== '=')
    defIndex [] = ["index.html", "index.htm"]
    defIndex x = x

-- | Run with the given middleware and parsing options from the command line.
--
-- Since 2.0.1
runCommandLine :: (Args -> Middleware) -> IO ()
runCommandLine middleware = do
    clArgs@Args{..} <- execParser $ info (helperOption <*> args) fullDesc
    let mime' = map (pack *** S8.pack) mime
    let mimeMap = Map.fromList mime' `Map.union` defaultMimeMap
    docroot' <- canonicalizePath docroot
    unless quiet $
        printf
            "Serving directory %s on port %d with %s index files.\n"
            docroot'
            port
            (if noindex then "no" else show index)
    let middle =
            gzip def{gzipFiles = GzipCompress}
                . (if verbose then logStdout else id)
                . middleware clArgs
    runSettings
        ( setPort port $
            setHost
                (fromString host)
                defaultSettings
        )
        $ middle
        $ staticApp
            (defaultFileServerSettings $ fromString docroot)
                { ssIndices = if noindex then [] else mapMaybe (toPiece . pack) index
                , ssGetMimeType =
                    return . mimeByExt mimeMap defaultMimeType . fromPiece . fileName
                }
  where
    helperOption :: Parser (a -> a)
    helperOption =
#if MIN_VERSION_optparse_applicative(0,16,0)
        abortOption (ShowHelpText Nothing) $
#else
        abortOption ShowHelpText $
#endif
            mconcat [long "help", help "Show this help text", hidden]
