{-# LANGUAGE OverloadedStrings #-}

import Data.String
import Data.Text (pack)
import Data.Maybe (mapMaybe)
import WaiAppStatic.Types (ssIndices, toPiece)
import Network.Wai.Application.Static (staticApp,defaultFileServerSettings)
import Network.Wai.Handler.Warp (runSettings, settingsPort, defaultSettings)
  
main :: IO ()
main = runSettings defaultSettings
    {
        settingsPort = 3000
    } $ staticApp (defaultFileServerSettings $ fromString ".")
    {
      ssIndices = mapMaybe (toPiece . pack) ["index.html"]
    }
