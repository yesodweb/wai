module Main where

import Test.DocTest

main :: IO ()
main = doctest [
    "-idist/build/autogen/"
  , "-optP-include"
  , "-optPdist/build/autogen/cabal_macros.h"
  , "Network/Wai/Handler/Warp.hs"
  ]
