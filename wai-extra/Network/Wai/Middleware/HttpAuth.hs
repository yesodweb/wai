{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
-- | Implements HTTP Basic Authentication.
--
-- This module may add digest authentication in the future.
module Network.Wai.Middleware.HttpAuth
    ( basicAuth
    , CheckCreds
    , AuthSettings
    , authRealm
    , authOnNoAuth
    , authIsProtected
    ) where

import Network.Wai
import Network.HTTP.Types (status401)
import Data.ByteString (ByteString)
import qualified Data.ByteString as S
import Data.String (IsString (..))
import Data.Word8 (isSpace, _colon, toLower)
import Data.ByteString.Base64 (decodeLenient)

-- | Check if a given username and password is valid.
type CheckCreds = ByteString
               -> ByteString
               -> IO Bool

-- | Perform basic authentication.
--
-- > basicAuth (\u p -> return $ u == "michael" && p == "mypass") "My Realm"
--
-- Since 1.3.4
basicAuth :: CheckCreds
          -> AuthSettings
          -> Middleware
basicAuth checkCreds AuthSettings {..} app req sendResponse = do
    isProtected <- authIsProtected req
    allowed <- if isProtected then check else return True
    if allowed
        then app req sendResponse
        else authOnNoAuth authRealm req sendResponse
  where
    check =
        case lookup "Authorization" $ requestHeaders req of
            Nothing -> return False
            Just bs ->
                let (x, y) = S.break isSpace bs
                 in if S.map toLower x == "basic"
                        then checkB64 $ S.dropWhile isSpace y
                        else return False
    checkB64 encoded =
        case S.uncons password' of
            Just (_, password) -> checkCreds username password
            Nothing -> return False
      where
        raw = decodeLenient encoded
        (username, password') = S.breakByte _colon raw

-- | Authentication settings. This value is an instance of @IsString@, so the
-- recommended approach to create a value is to provide a string literal (which
-- will be the realm) and then overriding individual fields.
--
-- > "My Realm" { authIsProtected = someFunc } :: AuthSettings
--
-- Since 1.3.4
data AuthSettings = AuthSettings
    { authRealm :: !ByteString
    -- ^
    --
    -- Since 1.3.4
    , authOnNoAuth :: !(ByteString -> Application)
    -- ^ Takes the realm and returns an appropriate 401 response when
    -- authentication is not provided.
    --
    -- Since 1.3.4
    , authIsProtected :: !(Request -> IO Bool)
    -- ^ Determine if access to the requested resource is restricted.
    --
    -- Default: always returns @True@.
    --
    -- Since 1.3.4
    }

instance IsString AuthSettings where
    fromString s = AuthSettings
        { authRealm = fromString s
        , authOnNoAuth = \realm _req f -> f $ responseLBS
            status401
            [ ("Content-Type", "text/plain")
            , ("WWW-Authenticate", S.concat
                [ "Basic realm=\""
                , realm
                , "\""
                ])
            ]
            "Basic authentication is required"
        , authIsProtected = const $ return True
        }
