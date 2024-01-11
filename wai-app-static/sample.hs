{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe (mapMaybe)
import Data.String
import Data.Text (pack)
import Network.Wai.Application.Static (defaultFileServerSettings, staticApp)
import Network.Wai.Handler.Warp (defaultSettings, runSettings, settingsPort)
import WaiAppStatic.Types (ssIndices, toPiece)

main :: IO ()
main =
    runSettings
        defaultSettings
            { settingsPort = 3000
            }
        $ staticApp
            (defaultFileServerSettings $ fromString ".")
                { ssIndices = mapMaybe (toPiece . pack) ["index.html"]
                }
