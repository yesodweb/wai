{-# LANGUAGE CPP  #-}
#if __GLASGOW_HASKELL__ < 900 || __GLASGOW_HASKELL__ >= 902
import Test.DocTest
#endif

main :: IO ()
main =
#if __GLASGOW_HASKELL__ < 900
    doctest ["Network"]
#else
#if __GLASGOW_HASKELL__ >= 902
    doctest ["Network"]
#else
    putStrLn "Doesn't work on GHC 9.0.*"
#endif
#endif
