{-# LANGUAGE OverloadedStrings, MagicHash, BangPatterns  #-}

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
import Data.Maybe (fromMaybe)

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as B
import qualified Data.Char as C
import qualified Numeric as N
import qualified Test.QuickCheck as QC
import qualified Data.ByteString.Lex.Integral as LI

import GHC.Prim
import GHC.Types

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


-- Use ByteString's readInteger function (which requires some extra stuff to
-- slow us down).
readIntegerBS :: ByteString -> Integer
readIntegerBS bs = fst $ fromMaybe (0, "") $ B.readInteger bs


-- No checking for non-digits. Will overflow at 2^31 on 32 bit CPUs.
readIntRaw :: ByteString -> Int
readIntRaw =
    B.foldl' (\i c -> i * 10 + C.digitToInt c) 0


-- The first good solution.
readInt64 :: ByteString -> Int64
readInt64 bs =
    B.foldl' (\i c -> i * 10 + fromIntegral (C.digitToInt c)) (0::Int64)
            $ B.takeWhile C.isDigit bs

readInt :: ByteString -> Int
readInt bs = fromIntegral $ readInt64 bs

readInteger :: ByteString -> Integer
readInteger bs = fromIntegral $ readInt64 bs


-- MagicHash version suggested by Vincent Hanquez.
readIntMH :: Integral a => ByteString -> a
readIntMH s = fromIntegral $ ireadInt64MH s
  where
    ireadInt64MH :: ByteString -> Int64
    ireadInt64MH bs =
        B.foldl' (\i c -> i * 10 + fromIntegral (mhDigitToInt c)) 0
             $ B.takeWhile C.isDigit bs

readInt64MH :: ByteString -> Int64
readInt64MH = readIntMH

readIntegerMH :: ByteString -> Integer
readIntegerMH = readIntMH

data Table = Table !Addr#

mhDigitToInt :: Char -> Int
mhDigitToInt (C# i) = I# (word2Int# (indexWord8OffAddr# addr (ord# i)))
  where
    !(Table addr) = table
    table :: Table
    table = Table
        "\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\
        \\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00"#


readIntBSL :: ByteString -> Int
readIntBSL = LI.readDecimal_

readInt64BSL :: ByteString -> Int64
readInt64BSL = LI.readDecimal_


-- A QuickCheck property. Test that for a number >= 0, converting it to
-- a string using show and then reading the value back with the function
-- under test returns the original value.
-- The functions under test only work on Natural numbers (the Conent-Length
-- field in a HTTP header is always >= 0) so we check the absolute value of
-- the value that QuickCheck generates for us.
prop_read_show_idempotent :: (Integral a, Show a) => (ByteString -> a) -> a -> Bool
prop_read_show_idempotent freader x =
    let px = abs x
    in px == freader (B.pack $ show px)


runQuickCheckTests :: IO ()
runQuickCheckTests = do
    QC.quickCheck (prop_read_show_idempotent readInt)
    QC.quickCheck (prop_read_show_idempotent readInt64)
    QC.quickCheck (prop_read_show_idempotent readInteger)
    QC.quickCheck (prop_read_show_idempotent readInt64MH)
    QC.quickCheck (prop_read_show_idempotent readIntegerMH)
    QC.quickCheck (prop_read_show_idempotent readIntBSL)
    QC.quickCheck (prop_read_show_idempotent readInt64BSL)

runCriterionTests :: ByteString -> IO ()
runCriterionTests number =
    defaultMain
       [ bench "readIntOrig"   $ nf readIntOrig number
       , bench "readDec"       $ nf readDec number
       , bench "readIntegerBS" $ nf readIntegerBS number
       , bench "readRaw"       $ nf readIntRaw number
       , bench "readInt"       $ nf readInt number
       , bench "readInt64"     $ nf readInt64 number
       , bench "readInteger"   $ nf readInteger number
       , bench "readInt64MH"   $ nf readInt64MH number
       , bench "readIntegerMH" $ nf readIntegerMH number

       -- Current best.
       , bench "readIntBSL"    $ nf readIntBSL number
       , bench "readInt64BSL"  $ nf readInt64BSL number
       ]

main :: IO ()
main = do
    putStrLn "Quickcheck tests."
    runQuickCheckTests
    putStrLn "Criterion tests."
    runCriterionTests "1234567898765432178979128361238162386182"

