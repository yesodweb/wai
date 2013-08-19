{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Vhost (vhost, redirectWWW) where

import Network.Wai

import Network.HTTP.Types as H
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import qualified Data.ByteString as BS

vhost :: [(Request -> Bool, Application)] -> Application -> Application
vhost vhosts def req =
    case filter (\(b, _) -> b req) vhosts of
        [] -> def req
        (_, app):_ -> app req

redirectWWW :: Text -> Application -> Application -- W.MiddleWare
redirectWWW home app req =
  if maybe True (BS.isPrefixOf "www") $ lookup "host" $ requestHeaders req
    then return $ responseLBS H.status301
          [ ("Content-Type", "text/plain") , ("Location", TE.encodeUtf8 home) ] "Redirect"
    else app req

