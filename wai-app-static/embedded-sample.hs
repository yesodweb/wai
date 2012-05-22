{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import Data.FileEmbed
import WaiAppStatic.Types
import WaiAppStatic.Storage.Embedded

main :: IO ()
main = run 3000 $ staticApp (embeddedSettings $(embedDir "test"))
    { ssIndices = []
    , ssMaxAge = NoMaxAge
    }
