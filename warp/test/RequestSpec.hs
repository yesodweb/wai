{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RequestSpec (main, spec) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import Data.IORef
import Data.Monoid (Sum (..))
import qualified Network.HTTP.Types.Header as HH
import Network.Wai.Handler.Warp.File (parseByteRanges)
import Network.Wai.Handler.Warp.Request
import Network.Wai.Handler.Warp.Settings (
    defaultSettings,
    settingsMaxTotalHeaderLength,
 )
import Network.Wai.Handler.Warp.Types
import Test.Hspec
import Test.Hspec.QuickCheck

main :: IO ()
main = hspec spec

defaultMaxTotalHeaderLength :: Int
defaultMaxTotalHeaderLength = settingsMaxTotalHeaderLength defaultSettings

spec :: Spec
spec = do
    describe "headerLines" $ do
        it "takes until blank" $
            blankSafe `shouldReturn` ("", ["foo", "bar", "baz"])
        it "ignored leading whitespace in bodies" $
            whiteSafe `shouldReturn` (" hi there", ["foo", "bar", "baz"])
        it "throws OverLargeHeader when too many" $
            tooMany `shouldThrow` overLargeHeader
        it "throws OverLargeHeader when too large" $
            tooLarge `shouldThrow` overLargeHeader
        it "known bad chunking behavior #239" $ do
            let chunks =
                    [ "GET / HTTP/1.1\r\nConnection: Close\r"
                    , "\n\r\n"
                    ]
            (actual, src) <- headerLinesList' chunks
            leftover <- readLeftoverSource src
            leftover `shouldBe` S.empty
            actual `shouldBe` ["GET / HTTP/1.1", "Connection: Close"]
        prop "random chunking" $ \breaks extraS -> do
            let bsFull = "GET / HTTP/1.1\r\nConnection: Close\r\n\r\n" `S8.append` extra
                extra = S8.pack extraS
                chunks = loop breaks bsFull
                loop [] bs = [bs, undefined]
                loop (x : xs) bs =
                    bs1 : loop xs bs2
                  where
                    (bs1, bs2) = S8.splitAt ((x `mod` 10) + 1) bs
            (actual, src) <- headerLinesList' chunks
            leftover <- consumeLen (length extraS) src

            actual `shouldBe` ["GET / HTTP/1.1", "Connection: Close"]
            leftover `shouldBe` extra
    describe "parseByteRanges" $ do
        let test x y = it x $ parseByteRanges (S8.pack x) `shouldBe` y
        test "bytes=0-499" $ Just [HH.ByteRangeFromTo 0 499]
        test "bytes=500-999" $ Just [HH.ByteRangeFromTo 500 999]
        test "bytes=-500" $ Just [HH.ByteRangeSuffix 500]
        test "bytes=9500-" $ Just [HH.ByteRangeFrom 9500]
        test "foobytes=9500-" Nothing
        test "bytes=0-0,-1" $ Just [HH.ByteRangeFromTo 0 0, HH.ByteRangeSuffix 1]

    describe "headerLines" $ do
        let parseHeaderLine chunks = do
                src <- mkSourceFunc chunks >>= mkSource
                x <- headerLines defaultMaxTotalHeaderLength FirstRequest src
                x `shouldBe` ["Status: 200", "Content-Type: text/plain"]

        it "can handle a normal case" $
            parseHeaderLine ["Status: 200\r\nContent-Type: text/plain\r\n\r\n"]

        it "can handle a nasty case (1)" $ do
            parseHeaderLine ["Status: 200", "\r\nContent-Type: text/plain", "\r\n\r\n"]
            parseHeaderLine ["Status: 200\r\n", "Content-Type: text/plain", "\r\n\r\n"]

        it "can handle a nasty case (2)" $ do
            parseHeaderLine
                ["Status: 200", "\r", "\nContent-Type: text/plain", "\r", "\n\r\n"]

        it "can handle a nasty case (3)" $ do
            parseHeaderLine
                ["Status: 200", "\r", "\n", "Content-Type: text/plain", "\r", "\n", "\r", "\n"]

        it "can handle a stupid case (3)" $
            parseHeaderLine $
                S8.pack . (: []) <$> "Status: 200\r\nContent-Type: text/plain\r\n\r\n"

        it "can (not) handle an illegal case (1)" $ do
            let chunks = ["\nStatus:", "\n 200", "\nContent-Type: text/plain", "\r\n\r\n"]
            src <- mkSourceFunc chunks >>= mkSource
            x <- headerLines defaultMaxTotalHeaderLength FirstRequest src
            x `shouldBe` []
            y <- headerLines defaultMaxTotalHeaderLength FirstRequest src
            y `shouldBe` ["Status:", " 200", "Content-Type: text/plain"]

        let testLengthHeaders = ["Sta", "tus: 200\r", "\n", "Content-Type: ", "text/plain\r\n\r\n"]
            headerLength = getSum $ foldMap (Sum . S.length) testLengthHeaders
            testLength = headerLength - 2 -- Because the second CRLF at the end isn't counted
        -- Length is 39, this shouldn't fail
        it "doesn't throw on correct length" $ do
            src <- mkSourceFunc testLengthHeaders >>= mkSource
            x <- headerLines testLength FirstRequest src
            x `shouldBe` ["Status: 200", "Content-Type: text/plain"]
        -- Length is still 39, this should fail
        it "throws error on correct length too long" $ do
            src <- mkSourceFunc testLengthHeaders >>= mkSource
            headerLines (testLength - 1) FirstRequest src `shouldThrow` (== OverLargeHeader)
  where
    blankSafe = headerLinesList ["f", "oo\n", "bar\nbaz\n\r\n"]
    whiteSafe = headerLinesList ["foo\r\nbar\r\nbaz\r\n\r\n hi there"]
    tooMany = headerLinesList $ repeat "f\n"
    tooLarge = headerLinesList $ repeat "f"

headerLinesList :: [S8.ByteString] -> IO (S8.ByteString, [S8.ByteString])
headerLinesList orig = do
    (res, src) <- headerLinesList' orig
    leftover <- readLeftoverSource src
    return (leftover, res)

headerLinesList' :: [S8.ByteString] -> IO ([S8.ByteString], Source)
headerLinesList' orig = do
    ref <- newIORef orig
    let src = do
            x <- readIORef ref
            case x of
                [] -> return S.empty
                y : z -> do
                    writeIORef ref z
                    return y
    src' <- mkSource src
    res <- headerLines defaultMaxTotalHeaderLength FirstRequest src'
    return (res, src')

consumeLen :: Int -> Source -> IO S8.ByteString
consumeLen len0 src =
    loop id len0
  where
    loop front len
        | len <= 0 = return $ S.concat $ front []
        | otherwise = do
            bs <- readSource src
            if S.null bs
                then loop front 0
                else do
                    let (x, _) = S.splitAt len bs
                    loop (front . (x :)) (len - S.length x)

overLargeHeader :: Selector InvalidRequest
overLargeHeader e = e == OverLargeHeader

mkSourceFunc :: [S8.ByteString] -> IO (IO S8.ByteString)
mkSourceFunc bss = do
    ref <- newIORef bss
    return $ reader ref
  where
    reader ref = do
        xss <- readIORef ref
        case xss of
            [] -> return S.empty
            (x : xs) -> do
                writeIORef ref xs
                return x
