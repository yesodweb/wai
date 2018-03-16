module Network.WaiSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Network.Wai
import Data.IORef
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder (Builder, toLazyByteString, word8)
import Control.Monad (forM_)

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
                    fmap (L.toStrict . toLazyByteString) $ readIORef builderRef
        prop "responseLBS" $ \bytes -> do
            body <- getBody $ responseLBS undefined undefined $ L.pack bytes
            body `shouldBe` S.pack bytes
        prop "responseBuilder" $ \bytes -> do
            body <- getBody $ responseBuilder undefined undefined
                            $ mconcat $ map word8 bytes
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
            body <- getBody $ responseFile undefined undefined fp $ Just FilePart
                { filePartOffset = fromIntegral offset
                , filePartByteCount = fromIntegral count
                , filePartFileSize = fromIntegral total
                }
            let expected = S.take count $ S.drop offset totalBS
            body `shouldBe` expected
    describe "lazyRequestBody" $ do
        prop "works" $ \chunks -> do
            ref <- newIORef $ map S.pack $ filter (not . null) chunks
            let req = defaultRequest
                        { requestBody = atomicModifyIORef ref $ \bss ->
                            case bss of
                                [] -> ([], S.empty)
                                x:y -> (y, x)
                        }
            body <- lazyRequestBody req
            body `shouldBe` L.fromChunks (map S.pack chunks)
        it "is lazy" $ do
            let req = defaultRequest
                        { requestBody = error "requestBody"
                        }
            _ <- lazyRequestBody req
            return ()
    describe "strictRequestBody" $ do
        prop "works" $ \chunks -> do
            ref <- newIORef $ map S.pack $ filter (not . null) chunks
            let req = defaultRequest
                        { requestBody = atomicModifyIORef ref $ \bss ->
                            case bss of
                                [] -> ([], S.empty)
                                x:y -> (y, x)
                        }
            body <- strictRequestBody req
            body `shouldBe` L.fromChunks (map S.pack chunks)
