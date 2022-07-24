{-# LANGUAGE CPP  #-}
import Test.DocTest

main :: IO ()
main =
#if __GLASGOW_HASKELL__ < 900
    doctest ["Network"]
#else
    putStrLn "Only works on GHC < 9.0"
#endif
