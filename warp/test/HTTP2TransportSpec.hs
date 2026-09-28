{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module HTTP2TransportSpec (spec) where

import Data.ByteString (ByteString)
import Network.Wai.Handler.Warp.HTTP2.Types (isHTTP2)
import Network.Wai.Handler.Warp.Types (Transport (..))
import Test.Hspec

spec :: Spec
spec = describe "HTTP/2 ALPN transport dispatch" $ do
    it "leaves plain TCP to prior-knowledge detection" $
        isHTTP2 TCP `shouldBe` False
    it "does not select HTTP/2 without TLS ALPN" $
        isHTTP2 (tls Nothing) `shouldBe` False
    it "does not select HTTP/2 for HTTP/1.1 ALPN" $
        isHTTP2 (tls (Just "http/1.1")) `shouldBe` False
    it "selects HTTP/2 for h2 ALPN" $
        isHTTP2 (tls (Just "h2")) `shouldBe` True
    it "preserves the existing h2 prefix behavior" $
        isHTTP2 (tls (Just "h2-14")) `shouldBe` True
    it "does not use the HTTP/2 ALPN path for QUIC" $
        isHTTP2 (quic (Just "h3")) `shouldBe` False
    it "does not inspect TLS fields for QUIC without ALPN" $
        isHTTP2 (quic Nothing) `shouldBe` False
    it "does not treat QUIC with an h2 label as TLS" $
        isHTTP2 (quic (Just "h2")) `shouldBe` False

tls :: Maybe ByteString -> Transport
tls protocol = TLS 3 3 protocol 0
#ifdef MIN_VERSION_crypton_x509
    Nothing
#endif

quic :: Maybe ByteString -> Transport
quic protocol = QUIC protocol 0
#ifdef MIN_VERSION_crypton_x509
    Nothing
#endif
