{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
import Network.Wai.Application.Static (staticApp, defaultFileServerSettings)
import Network.Wai.Handler.Warp
    ( runSettings, defaultSettings, settingsHost, settingsPort
    )
import System.Console.CmdArgs hiding (def)
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
