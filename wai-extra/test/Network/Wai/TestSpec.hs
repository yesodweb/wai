{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.TestSpec (main, spec) where

import           Test.Hspec

import           Network.Wai
import           Network.Wai.Test

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "setPath" $ do

    let req = setPath defaultRequest "/foo/bar/baz?foo=23&bar=42&baz"

    it "sets pathInfo" $ do
      pathInfo req `shouldBe` ["foo", "bar", "baz"]

    it "utf8 path" $
      pathInfo (setPath defaultRequest "/foo/%D7%A9%D7%9C%D7%95%D7%9D/bar") `shouldBe`
        ["foo", "שלום", "bar"]

    it "sets rawPathInfo" $ do
      rawPathInfo req `shouldBe` "/foo/bar/baz"

    it "sets queryString" $ do
      queryString req `shouldBe` [("foo", Just "23"), ("bar", Just "42"), ("baz", Nothing)]

    it "sets rawQueryString" $ do
      rawQueryString req `shouldBe` "?foo=23&bar=42&baz"

    context "when path has no query string" $ do
      it "sets rawQueryString to empty string" $ do
        rawQueryString (setPath defaultRequest "/foo/bar/baz") `shouldBe` ""
