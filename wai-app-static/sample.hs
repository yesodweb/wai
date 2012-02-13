{-# LANGUAGE OverloadedStrings #-}
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp (run)

main :: IO ()
main = run 3000 $ staticApp defaultWebAppSettings
    { ssFolder = fileSystemLookup "."
    , ssMaxAge = MaxAgeForever
    , ssIndices = []
    }
