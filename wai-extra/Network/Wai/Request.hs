-- | Some helpers for interrogating a WAI 'Request'.

module Network.Wai.Request
    ( appearsSecure
    , guessApproot
    ) where

import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Network.HTTP.Types (HeaderName)
import Network.Wai (Request, isSecure, requestHeaders, requestHeaderHost)

import qualified Data.ByteString as S
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
--
-- Since 3.0.7
appearsSecure :: Request -> Bool
appearsSecure request = isSecure request || any (uncurry matchHeader)
    [ ("HTTPS"                  , (== "on"))
    , ("HTTP_X_FORWARDED_SSL"   , (== "on"))
    , ("HTTP_X_FORWARDED_SCHEME", (== "https"))
    , ("HTTP_X_FORWARDED_PROTO" , ((== ["https"]) . take 1 . C.split ','))
    , ("X-Forwarded-Proto"      , (== "https")) -- Used by Nginx and AWS ELB.
    ]

  where
    matchHeader :: HeaderName -> (ByteString -> Bool) -> Bool
    matchHeader h f = maybe False f $ lookup h $ requestHeaders request

-- | Guess the \"application root\" based on the given request.
--
-- The application root is the basis for forming URLs pointing at the current
-- application. For more information and relevant caveats, please see
-- "Network.Wai.Middleware.Approot".
--
-- Since 3.0.7
guessApproot :: Request -> ByteString
guessApproot req =
    (if appearsSecure req then "https://" else "http://") `S.append`
    (fromMaybe "localhost" $ requestHeaderHost req)
