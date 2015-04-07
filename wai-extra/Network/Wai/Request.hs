{-# LANGUAGE OverloadedStrings #-}
-- | Some helpers for interrogating a WAI 'Request'.

module Network.Wai.Request
    ( appearsSecure
    ) where

import Data.ByteString (ByteString)
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request, isSecure, requestHeaders)

import qualified Data.ByteString.Char8 as C

-- | Does this request appear to have been made over an SSL connection?
--
-- This function first checks @'isSecure'@, but also checks for headers that may
-- indicate a secure connection even in the presence of reverse proxies.
--
-- Note: these headers can be easily spoofed, so decisions which require a true
-- SSL connection (i.e. sending sensitive information) should only use
-- @'isSecure'@. This is not always the case though: for example, deciding to
-- force a non-SSL request to SSL by redirect. One can safely choose not to
-- redirect when the request /appears/ secure, even if it's actually not.
appearsSecure :: Request -> Bool
appearsSecure request = isSecure request || any (uncurry matchHeader)
    [ ("HTTPS"                  , (== "on"))
    , ("HTTP_X_FORWARDED_SSL"   , (== "on"))
    , ("HTTP_X_FORWARDED_SCHEME", (== "https"))
    , ("HTTP_X_FORWARDED_PROTO" , ((== ["https"]) . take 1 . C.split ','))
    ]

  where
    matchHeader :: HeaderName -> (ByteString -> Bool) -> Bool
    matchHeader h f = maybe False f $ lookup h $ requestHeaders request
