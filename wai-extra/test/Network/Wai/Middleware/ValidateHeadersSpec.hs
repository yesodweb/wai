{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.ValidateHeadersSpec (spec) where

import Network.HTTP.Types (ResponseHeaders, status200)
import Network.Wai (Application, defaultRequest, responseLBS)
import Test.Hspec (Spec, describe, it)

import Network.Wai.Middleware.ValidateHeaders (validateHeadersMiddleware, defaultValidateHeadersSettings)
import Network.Wai.Test (Session, assertStatus, request, withSession)

spec :: Spec
spec = do
    describe "validateHeadersMiddleware" $ do
        it "allows token characters in header names" $ withHeadersApp [("token!#$%&'*+-.^_`|~123", "bar")] $ do
            assertStatus 200 =<< request defaultRequest

        it "does not allow colons in header names" $ withHeadersApp [("broken:header", "foo")] $ do
            assertStatus 500 =<< request defaultRequest

        it "does not allow whitespace in header names" $ do
            withHeadersApp [("white space", "foo")] $ assertStatus 500 =<< request defaultRequest
            withHeadersApp [("white\nspace", "foo")] $ assertStatus 500 =<< request defaultRequest
            withHeadersApp [("white\rspace", "foo")] $ assertStatus 500 =<< request defaultRequest
            withHeadersApp [("white\tspace", "foo")] $ assertStatus 500 =<< request defaultRequest
            withHeadersApp [("ehite\vspace", "foo")] $ assertStatus 500 =<< request defaultRequest
            withHeadersApp [("white\fspace", "foo")] $ assertStatus 500 =<< request defaultRequest

        it "allows visible ASCII, space and horizontal tab in header values" $ do
            withHeadersApp [("MyHeader", "the quick brown\tfox jumped over the lazy dog!")] $ do
                assertStatus 200 =<< request defaultRequest

        it "allows octets beyond 0x80 in headers values" $ do
            -- Just an example
            withHeadersApp [("MyHeader", "verr\252ckt")] $ do
                assertStatus 200 =<< request defaultRequest

        it "does not allow other whitespace in header values" $ do
            withHeadersApp [("MyHeader", "white\nspace")] $ assertStatus 500 =<< request defaultRequest
            withHeadersApp [("MyHeader", "white\rspace")] $ assertStatus 500 =<< request defaultRequest
            withHeadersApp [("MyHeader", "white\vspace")] $ assertStatus 500 =<< request defaultRequest
            withHeadersApp [("MyHeader", "white\fspace")] $ assertStatus 500 =<< request defaultRequest

        it "does not allow control characters in header values" $ do
            -- Just examples again
            withHeadersApp [("MyHeader", "control character \0")] $ assertStatus 500 =<< request defaultRequest
            withHeadersApp [("MyHeader", "control character \27")] $ assertStatus 500 =<< request defaultRequest

        it "does not allow trailing whitespace in header values" $ do
            withHeadersApp [("MyHeader", " foo")] $ assertStatus 500 =<< request defaultRequest
            withHeadersApp [("MyHeader", "foo ")] $ assertStatus 500 =<< request defaultRequest

withHeadersApp :: ResponseHeaders -> Session a -> IO a
withHeadersApp headers session =
  withSession (validateHeadersMiddleware defaultValidateHeadersSettings $ headersApp headers) session

headersApp :: ResponseHeaders -> Application
headersApp headers _ respond =
  respond $ responseLBS status200 headers ""
