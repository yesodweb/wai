{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import Data.FileEmbed
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)
import WaiAppStatic.Storage.Embedded
import WaiAppStatic.Types

main :: IO ()
main =
    run 3000 $
        staticApp
            (embeddedSettings $(embedDir "test"))
                { ssIndices = []
                , ssMaxAge = NoMaxAge
                }
