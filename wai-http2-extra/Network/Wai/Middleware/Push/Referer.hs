{-# LANGUAGE OverloadedStrings #-}

-- | Middleware for server push learning dependency based on Referer:.
module Network.Wai.Middleware.Push.Referer (
    -- * Middleware
    pushOnReferer,

    -- * Making push promise
    URLPath,
    MakePushPromise,
    defaultMakePushPromise,

    -- * Settings
    Settings,
    M.defaultSettings,
    makePushPromise,
    duration,
    keyLimit,
    valueLimit,
) where

import Control.Monad (when)
import qualified Data.ByteString as BS
import Data.Maybe (isNothing)
import Network.HTTP.Types (Status (..))
import Network.Wai
import Network.Wai.Handler.Warp hiding (Settings, defaultSettings)
import Network.Wai.Internal (Response (..))

import qualified Network.Wai.Middleware.Push.Referer.Manager as M
import Network.Wai.Middleware.Push.Referer.ParseURL
import Network.Wai.Middleware.Push.Referer.Types

-- $setup
-- >>> :set -XOverloadedStrings

-- | The middleware to push files based on Referer:.
--   Learning strategy is implemented in the first argument.
pushOnReferer :: Settings -> Middleware
pushOnReferer settings app req sendResponse = do
    mgr <- M.getManager settings
    app req $ push mgr
  where
    path = rawPathInfo req
    push mgr res@(ResponseFile (Status 200 "OK") _ file Nothing)
        -- file:    /index.html
        -- path:    /
        -- referer:
        -- refPath:
        | isHTML path = do
            xs <- M.lookup path mgr
            case xs of
                [] -> return ()
                ps -> do
                    let h2d = defaultHTTP2Data{http2dataPushPromise = ps}
                    setHTTP2Data req $ Just h2d
            sendResponse res
        -- file:    /style.css
        -- path:    /style.css
        -- referer: /index.html
        -- refPath: /
        | otherwise = case requestHeaderReferer req of
            Nothing -> sendResponse res
            Just referer -> do
                (mauth, refPath) <- parseUrl referer
                when
                    ( (isNothing mauth || requestHeaderHost req == mauth)
                        && path /= refPath
                        && isHTML refPath
                    )
                    $ do
                        let path' = BS.copy path
                            refPath' = BS.copy refPath
                        mpp <- makePushPromise settings refPath' path' file
                        case mpp of
                            Nothing -> return ()
                            Just pp -> M.insert refPath' pp mgr
                sendResponse res
    push _ res = sendResponse res

isHTML :: URLPath -> Bool
isHTML p =
    ("/" `BS.isSuffixOf` p)
        || (".html" `BS.isSuffixOf` p)
        || (".htm" `BS.isSuffixOf` p)
