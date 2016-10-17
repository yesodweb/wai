{-# LANGUAGE RecordWildCards, TupleSections, CPP #-}
-- | Implements HTTP Basic Authentication.
--
-- This module may add digest authentication in the future.
module Network.Wai.Middleware.HttpAuth
    ( -- * Middleware
      basicAuth
    , basicAuth'
    , CheckCreds
    , AuthSettings
    , authRealm
    , authOnNoAuth
    , authIsProtected
      -- * Helping functions
    , extractBasicAuth
    , extractBearerAuth
    ) where
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Data.ByteString (ByteString)
import Data.ByteString.Base64 (decodeLenient)
import Data.String (IsString (..))
import Data.Word8 (isSpace, _colon, toLower)
import Network.HTTP.Types (status401, hContentType, hAuthorization)
import Network.Wai

import qualified Data.ByteString as S


-- | Check if a given username and password is valid.
type CheckCreds = ByteString
               -> ByteString
               -> IO Bool

-- | Perform basic authentication.
--
-- > basicAuth (\u p -> return $ u == "michael" && p == "mypass") "My Realm"
--
-- @since 1.3.4
basicAuth :: CheckCreds
          -> AuthSettings
          -> Middleware
basicAuth checkCreds = basicAuth' (\_ -> checkCreds)

-- | Like 'basicAuth', but also passes a request to the authentication function.
--
-- @since 3.0.19
basicAuth' :: (Request -> CheckCreds)
           -> AuthSettings
           -> Middleware
basicAuth' checkCreds AuthSettings {..} app req sendResponse = do
    isProtected <- authIsProtected req
    allowed <- if isProtected then check else return True
    if allowed
        then app req sendResponse
        else authOnNoAuth authRealm req sendResponse
  where
    check =
        case (lookup hAuthorization $ requestHeaders req)
             >>= extractBasicAuth of
            Nothing -> return False
            Just (username, password) -> checkCreds req username password

-- | Basic authentication settings. This value is an instance of
-- @IsString@, so the recommended approach to create a value is to
-- provide a string literal (which will be the realm) and then
-- overriding individual fields.
--
-- > "My Realm" { authIsProtected = someFunc } :: AuthSettings
--
-- @since 1.3.4
data AuthSettings = AuthSettings
    { authRealm :: !ByteString
    -- ^
    --
    -- @since 1.3.4
    , authOnNoAuth :: !(ByteString -> Application)
    -- ^ Takes the realm and returns an appropriate 401 response when
    -- authentication is not provided.
    --
    -- @since 1.3.4
    , authIsProtected :: !(Request -> IO Bool)
    -- ^ Determine if access to the requested resource is restricted.
    --
    -- Default: always returns @True@.
    --
    -- @since 1.3.4
    }

instance IsString AuthSettings where
    fromString s = AuthSettings
        { authRealm = fromString s
        , authOnNoAuth = \realm _req f -> f $ responseLBS
            status401
            [ (hContentType, "text/plain")
            , ("WWW-Authenticate", S.concat
                [ "Basic realm=\""
                , realm
                , "\""
                ])
            ]
            "Basic authentication is required"
        , authIsProtected = const $ return True
        }

-- | Extract basic authentication data from usually __Authorization__
-- header value. Returns username and password
--
-- @since 3.0.5
extractBasicAuth :: ByteString -> Maybe (ByteString, ByteString)
extractBasicAuth bs =
    let (x, y) = S.break isSpace bs
    in if S.map toLower x == "basic"
       then extract $ S.dropWhile isSpace y
       else Nothing
  where
    extract encoded =
        let raw = decodeLenient encoded
            (username, password') = S.break (== _colon) raw
        in ((username,) . snd) <$> S.uncons password'

-- | Extract bearer authentication data from __Authorization__ header
-- value. Returns bearer token
--
-- @since 3.0.5
extractBearerAuth :: ByteString -> Maybe ByteString
extractBearerAuth bs =
    let (x, y) = S.break isSpace bs
    in if S.map toLower x == "bearer"
        then Just $ S.dropWhile isSpace y
        else Nothing
