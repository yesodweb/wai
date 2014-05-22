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

    it "sets rawPathInfo" $ do
      rawPathInfo req `shouldBe` "/foo/bar/baz"

    it "sets queryString" $ do
      queryString req `shouldBe` [("foo", Just "23"), ("bar", Just "42"), ("baz", Nothing)]

    it "sets rawQueryString" $ do
      rawQueryString req `shouldBe` "?foo=23&bar=42&baz"

    context "when path has no query string" $ do
      it "sets rawQueryString to empty string" $ do
        rawQueryString (setPath defaultRequest "/foo/bar/baz") `shouldBe` ""
