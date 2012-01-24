{-# LANGUAGE OverloadedStrings #-}

-- Copyright     : Erik de Castro Lopo <erikd@mega-nerd.com>
-- License       : BSD3

-- A program to QuickCheck and benchmark a function used in the Warp web server
-- and elsewhere to read the Content-Length field of HTTP headers.
--
-- Compile and run as:
--    ghc -Wall -O3 --make readInt.hs -o readInt && ./readInt

import Criterion.Main
import Data.ByteString (ByteString)
import Data.Int (Int64)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as C
import qualified Numeric as N
import qualified Test.QuickCheck as QC


-- This is the absolute mimimal solution. It will return garbage if the
-- imput string contains anything other than ASCI digits.
readIntOrig :: ByteString -> Integer
readIntOrig =
    S.foldl' (\x w -> x * 10 + fromIntegral w - 48) 0


-- Using Numeric.readDec which works on String, so the ByteString has to be
-- unpacked first.
readDec :: ByteString -> Integer
readDec s =
    case N.readDec (B.unpack s) of
        [] -> 0
        (x, _):_ -> x


-- No checking for non-digits. Will overflow at 2^31 on 32 bit CPUs.
readIntRaw :: ByteString -> Int
readIntRaw =
    B.foldl' (\i c -> i * 10 + C.digitToInt c) 0


-- The best solution.
readIntTC :: Integral a => ByteString -> a
readIntTC bs = fromIntegral
    $ B.foldl' (\i c -> i * 10 + C.digitToInt c) 0 $ B.takeWhile C.isDigit bs


-- Three specialisations of readIntTC.
readInt :: ByteString -> Int
readInt = readIntTC

readInt64 :: ByteString -> Int64
readInt64 = readIntTC

readInteger :: ByteString -> Integer
readInteger = readIntTC

-- A QuickCheck property. Test that for a number >= 0, converting it to
-- a string using show and then reading the value back with the function
-- under test returns the original value.
-- The functions under test only work on Natural numbers (the Conent-Length
-- field in a HTTP header is always >= 0) so we check the absolute value of
-- the value that QuickCheck generates for us.
prop_read_show_idempotent :: Integral a => (ByteString -> a) -> a -> Bool
prop_read_show_idempotent freader x =
    let px = abs x
    in px == freader (B.pack $ show px)


runQuickCheckTests :: IO ()
runQuickCheckTests = do
    QC.quickCheck (prop_read_show_idempotent readInt)
    QC.quickCheck (prop_read_show_idempotent readInt64)
    QC.quickCheck (prop_read_show_idempotent readInteger)


runCriterionTests :: ByteString -> IO ()
runCriterionTests number =
    defaultMain
       [ bench "readIntOrig" $ nf readIntOrig number
       , bench "readDec"     $ nf readDec number
       , bench "readRaw"     $ nf readIntRaw number
       , bench "readInt"     $ nf readInt number
       , bench "readInt64"   $ nf readInt64 number
       , bench "readInteger" $ nf readInteger number
       ]


main :: IO ()
main = do
    putStrLn "Quickcheck tests."
    runQuickCheckTests
    putStrLn "Criterion tests."
    runCriterionTests "1234567898765432178979128361238162386182"
