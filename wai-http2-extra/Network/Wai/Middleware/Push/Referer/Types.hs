{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.Push.Referer.Types (
    URLPath,
    MakePushPromise,
    defaultMakePushPromise,
    Settings (..),
    defaultSettings,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Network.Wai.Handler.Warp (PushPromise (..), defaultPushPromise)

-- | Type for URL path.
type URLPath = ByteString

-- | Making a push promise based on Referer:,
--   path to be pushed and file to be pushed.
--   If the middleware should push this file in the next time when
--   the page of Referer: is accessed,
--   this function should return 'Just'.
--   If 'Nothing' is returned,
--   the middleware learns nothing.
type MakePushPromise =
    URLPath
    -- ^ path in referer  (key: /index.html)
    -> URLPath
    -- ^ path to be pushed (value: /style.css)
    -> FilePath
    -- ^ file to be pushed (file_path/style.css)
    -> IO (Maybe PushPromise)

-- | Learn if the file to be pushed is CSS (.css) or JavaScript (.js) file.
defaultMakePushPromise :: MakePushPromise
defaultMakePushPromise refPath path file = case getCT path of
    Nothing -> return Nothing
    Just ct -> do
        let pp =
                defaultPushPromise
                    { promisedPath = path
                    , promisedFile = file
                    , promisedResponseHeaders =
                        [ ("content-type", ct)
                        , ("x-http2-push", refPath)
                        ]
                    }
        return $ Just pp

getCT :: URLPath -> Maybe ByteString
getCT p
    | ".js" `BS.isSuffixOf` p = Just "application/javascript"
    | ".css" `BS.isSuffixOf` p = Just "text/css"
    | otherwise = Nothing

-- | Settings for server push based on Referer:.
data Settings = Settings
    { makePushPromise :: MakePushPromise
    -- ^ Default: 'defaultMakePushPromise'
    , duration :: Int
    -- ^ Deprecated
    , keyLimit :: Int
    -- ^ Max number of keys (e.g. index.html) in the learning information. Default: 20
    , valueLimit :: Int
    -- ^ Max number of values (e.g. style.css) in the learning information. Default: 20
    }

-- | Default settings.
defaultSettings :: Settings
defaultSettings =
    Settings
        { makePushPromise = defaultMakePushPromise
        , duration = 0
        , keyLimit = 20
        , valueLimit = 20
        }
