{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai.Handler.Warp
    ( runSettings, defaultSettings, settingsHost, settingsPort
    )
import Options.Applicative
import Text.Printf (printf)
import System.Directory (canonicalizePath)
import Control.Monad (unless)
import Network.Wai.Middleware.Autohead
import Network.Wai.Middleware.RequestLogger (logStdout)
import Network.Wai.Middleware.Gzip
import qualified Data.Map as Map
import qualified Data.ByteString.Char8 as S8
import Control.Arrow ((***))
import Data.Text (pack)
import Data.String (fromString)
import Network.Mime (defaultMimeMap, mimeByExt, defaultMimeType)
import WaiAppStatic.Types (ssIndices, toPiece, ssGetMimeType, fileName, fromPiece)
import Data.Maybe (mapMaybe)
import Control.Arrow (second)

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

args :: Parser Args
args = Args
    <$> strOption
            ( long "docroot"
           <> short 'd'
           <> metavar "DOCROOT"
           <> value "."
           <> help "directory containing files to serve")
    <*> (defIndex <$> many (strOption
            ( long "index"
           <> short 'i'
           <> metavar "INDEX"
           <> help "index files to serve when a directory is required"
            )))
    <*> option
            ( long "port"
           <> short 'p'
           <> metavar "PORT"
           <> value 3000)
    <*> switch
            ( long "noindex"
           <> short 'n')
    <*> switch
            ( long "quiet"
           <> short 'q')
    <*> switch
            ( long "verbose"
           <> short 'v')
    <*> many (toPair <$> strOption
            ( long "mime"
           <> short 'm'
           <> metavar "MIME"
           <> help "extra file extension/mime type mappings"))
    <*> strOption
            ( long "host"
           <> short 'h'
           <> metavar "HOST"
           <> value "*"
           <> help "interface to bind to, special values: *, *4, *6")
  where
    toPair = second (drop 1) . break (== '=')
    defIndex [] = ["index.html", "index.htm"]
    defIndex x = x

main :: IO ()
main = do
    Args {..} <- execParser $ info (helper <*> args) fullDesc
    let mime' = map (pack *** S8.pack) mime
    let mimeMap = Map.fromList mime' `Map.union` defaultMimeMap
    docroot' <- canonicalizePath docroot
    unless quiet $ printf "Serving directory %s on port %d with %s index files.\n" docroot' port (if noindex then "no" else show index)
    let middle = gzip def { gzipFiles = GzipCompress }
               . (if verbose then logStdout else id)
               . autohead
    runSettings defaultSettings
        { settingsPort = port
        , settingsHost = fromString host
        } $ middle $ staticApp (defaultFileServerSettings $ fromString docroot)
        { ssIndices = if noindex then [] else mapMaybe (toPiece . pack) index
        , ssGetMimeType = return . mimeByExt mimeMap defaultMimeType . fromPiece . fileName
        }
