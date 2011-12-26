{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import Network.Wai.Application.Static
    ( StaticSettings (..), staticApp, defaultMimeType, defaultListing
    , defaultMimeTypes, mimeTypeByExt
    , defaultFileServerSettings, fileSystemLookup
    , fileName, toFilePath
    )
import Network.Wai.Handler.Warp
    ( runSettings, defaultSettings, settingsHost, settingsPort
    )
import System.Console.CmdArgs
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
    deriving (Show, Data, Typeable)

defaultArgs :: Args
defaultArgs = Args "." ["index.html", "index.htm"] 3000 False False False [] "*"

main :: IO ()
main = do
    Args {..} <- cmdArgs defaultArgs
    let mime' = map (toFilePath *** S8.pack) mime
    let mimeMap = Map.fromList mime' `Map.union` defaultMimeTypes
    docroot' <- canonicalizePath docroot
    unless quiet $ printf "Serving directory %s on port %d with %s index files.\n" docroot' port (if noindex then "no" else show index)
    let middle = gzip False
               . (if verbose then logStdout else id)
               . autohead
    runSettings defaultSettings
        { settingsPort = port
        , settingsHost = host
        } $ middle $ staticApp defaultFileServerSettings
        { ssFolder = fileSystemLookup $ toFilePath docroot
        , ssIndices = if noindex then [] else map pack index
        , ssListing = Just defaultListing
        , ssGetMimeType = return . mimeTypeByExt mimeMap defaultMimeType . fileName
        }
