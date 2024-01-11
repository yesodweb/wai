{-# LANGUAGE LambdaCase #-}

module Network.WaiSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as S
import Data.ByteString.Builder (Builder, toLazyByteString, word8)
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Data.Word (Word8)
import Network.Wai
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

spec :: Spec
spec = do
    describe "responseToStream" $ do
        let getBody res = do
                let (_, _, f) = responseToStream res
                f $ \streamingBody -> do
                    builderRef <- newIORef mempty
                    let add :: Builder -> IO ()
                        add b = atomicModifyIORef builderRef $ \builder ->
                            (builder `mappend` b, ())
                        flush :: IO ()
                        flush = return ()
                    streamingBody add flush
                    L.toStrict . toLazyByteString <$> readIORef builderRef
        prop "responseLBS" $ \bytes -> do
            body <- getBody $ responseLBS undefined undefined $ L.pack bytes
            body `shouldBe` S.pack bytes
        prop "responseBuilder" $ \bytes -> do
            body <-
                getBody $
                    responseBuilder undefined undefined $
                        mconcat $
                            map word8 bytes
            body `shouldBe` S.pack bytes
        prop "responseStream" $ \chunks -> do
            body <- getBody $ responseStream undefined undefined $ \sendChunk _ ->
                forM_ chunks $ \chunk -> sendChunk $ mconcat $ map word8 chunk
            body `shouldBe` S.concat (map S.pack chunks)
        it "responseFile total" $ do
            let fp = "wai.cabal"
            body <- getBody $ responseFile undefined undefined fp Nothing
            expected <- S.readFile fp
            body `shouldBe` expected
        prop "responseFile partial" $ \offset' count' -> do
            let fp = "wai.cabal"
            totalBS <- S.readFile fp
            let total = S.length totalBS
                offset = abs offset' `mod` total
                count = abs count' `mod` (total - offset)
            body <-
                getBody $
                    responseFile undefined undefined fp $
                        Just
                            FilePart
                                { filePartOffset = fromIntegral offset
                                , filePartByteCount = fromIntegral count
                                , filePartFileSize = fromIntegral total
                                }
            let expected = S.take count $ S.drop offset totalBS
            body `shouldBe` expected
    describe "lazyRequestBody" $ do
        prop "works" $ \chunks -> do
            req <- mkRequestFromChunks chunks
            body <- lazyRequestBody req
            body `shouldBe` L.fromChunks (map S.pack chunks)
        it "is lazy" $ do
            let req = setRequestBodyChunks (error "requestBody") defaultRequest
            _ <- lazyRequestBody req
            return ()
    describe "strictRequestBody" $ do
        prop "works" $ \chunks -> do
            req <- mkRequestFromChunks chunks
            body <- strictRequestBody req
            body `shouldBe` L.fromChunks (map S.pack chunks)

mkRequestFromChunks :: [[Word8]] -> IO Request
mkRequestFromChunks chunks = do
    ref <- newIORef $ map S.pack $ filter (not . null) chunks
    pure $
        flip setRequestBodyChunks defaultRequest $
            atomicModifyIORef ref $ \case
                [] -> ([], S.empty)
                x : y -> (y, x)
