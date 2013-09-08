{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}

module EmbeddedTestEntries where

import WaiAppStatic.Storage.Embedded
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString.Lazy as BL

body :: Int -> Char -> BL.ByteString
body i c = TL.encodeUtf8 $ TL.pack $ take i $ repeat c

mkEntries :: IO [EmbeddableEntry]
mkEntries = return
    -- An entry that should be compressed
  [ EmbeddableEntry "e1.txt" 
                    "text/plain" 
                    (Left ("Etag 1", body 1000 'A'))

    -- An entry so short that the compressed text is longer
  , EmbeddableEntry "e2.txt"
                    "text/plain"
                    (Left ("Etag 2", "ABC"))

    -- An entry that is not compressed because of the mime
  , EmbeddableEntry "somedir/e3.txt"
                    "xxx"
                    (Left ("Etag 3", body 1000 'A'))

    -- A reloadable entry
  , EmbeddableEntry "e4.css"
                    "text/css"
                    (Right [| return ("Etag 4" :: T.Text, body 2000 'Q') |])

    -- An entry without etag
  , EmbeddableEntry "e5.txt"
                    "text/plain"
                    (Left ("", body 1000 'Z'))

    -- A reloadable entry without etag
  , EmbeddableEntry "e6.txt"
                    "text/plain"
                    (Right [| return ("" :: T.Text, body 1000 'W') |] )
  ]
