module Main where

import Test.DocTest
import System.Environment (getEnvironment)

main :: IO ()
main = do
    env <- getEnvironment
    let dist =
            case lookup "HASKELL_DIST_DIR" env of
                Nothing -> "dist"
                Just x -> x
    doctest
        [ concat ["-i", dist, "/build/autogen/"]
        , "-optP-include"
        , concat ["-optP", dist, "/build/autogen/cabal_macros.h"]
        , "Network/Wai/Handler/Warp.hs"
        ]
