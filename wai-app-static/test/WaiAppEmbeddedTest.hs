{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module WaiAppEmbeddedTest (embSpec) where

import Codec.Compression.GZip (compress)
import EmbeddedTestEntries
import Network.Wai
import Network.Wai.Application.Static (staticApp)
import Network.Wai.Test
import Test.Hspec
import WaiAppStatic.Storage.Embedded

embApp :: Application
embApp = staticApp $(mkSettings mkEntries)

defRequest :: Request
defRequest = defaultRequest

embSpec :: Spec
embSpec = do
    let embed = flip runSession embApp
    describe "embedded, compressed entry" $ do
        it "served correctly" $ embed $ do
            req <- request (setRawPathInfo defRequest "e1.txt")
            assertStatus 200 req
            assertHeader "Content-Type" "text/plain" req
            assertHeader "Content-Encoding" "gzip" req
            assertHeader "ETag" "Etag 1" req
            assertNoHeader "Last-Modified req" req
            assertBody (compress $ body 1000 'A') req

        it "304 when valid if-none-match sent" $ embed $ do
            req <- request (setRawPathInfo defRequest "e1.txt")
                             { requestHeaders = [("If-None-Match", "Etag 1")] }
            assertStatus 304 req

    describe "embedded, uncompressed entry" $ do
        it "too short" $ embed $ do
            req <- request (setRawPathInfo defRequest "e2.txt")
            assertStatus 200 req
            assertHeader "Content-Type" "text/plain" req
            assertNoHeader "Content-Encoding" req
            assertHeader "ETag" "Etag 2" req
            assertBody "ABC" req

        it "wrong mime" $ embed $ do
            req <- request (setRawPathInfo defRequest "somedir/e3.txt")
            assertStatus 200 req
            assertHeader "Content-Type" "xxx" req
            assertNoHeader "Content-Encoding" req
            assertHeader "ETag" "Etag 3" req
            assertBody (body 1000 'A') req

    describe "reloadable entry" $
        it "served correctly" $ embed $ do
            req <- request (setRawPathInfo defRequest "e4.css")
            assertStatus 200 req
            assertHeader "Content-Type" "text/css" req
            assertNoHeader "Content-Encoding" req
            assertHeader "ETag" "Etag 4" req
            assertBody (body 2000 'Q') req

    describe "entries without etags" $ do
        it "embedded entry" $ embed $ do
            req <- request (setRawPathInfo defRequest "e5.txt")
            assertStatus 200 req
            assertHeader "Content-Type" "text/plain" req
            assertHeader "Content-Encoding" "gzip" req
            assertNoHeader "ETag" req
            assertBody (compress $ body 1000 'Z') req

        it "reload entry" $ embed $ do
            req <- request (setRawPathInfo defRequest "e6.txt")
            assertStatus 200 req
            assertHeader "Content-Type" "text/plain" req
            assertNoHeader "Content-Encoding" req
            assertNoHeader "ETag" req
            assertBody (body 1000 'W') req

