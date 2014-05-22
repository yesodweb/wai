{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Vhost (vhost, redirectWWW, redirectTo, redirectToLogged) where

import Network.Wai

import Network.HTTP.Types as H
import qualified Data.Text.Encoding as TE
import Data.Text (Text)
import qualified Data.ByteString as BS
import Data.Monoid (mappend)

vhost :: [(Request -> Bool, Application)] -> Application -> Application
vhost vhosts def req =
    case filter (\(b, _) -> b req) vhosts of
        [] -> def req
        (_, app):_ -> app req

redirectWWW :: Text -> Application -> Application -- W.MiddleWare
redirectWWW home =
  redirectIf home (maybe True (BS.isPrefixOf "www") . lookup "host" . requestHeaders)

redirectIf :: Text -> (Request -> Bool) -> Application -> Application
redirectIf home cond app req sendResponse =
  if cond req
    then sendResponse $ redirectTo $ TE.encodeUtf8 home
    else app req sendResponse

redirectTo :: BS.ByteString -> Response
redirectTo location = responseLBS H.status301
    [ ("Content-Type", "text/plain") , ("Location", location) ] "Redirect"

redirectToLogged :: (Text -> IO ()) -> BS.ByteString -> IO Response
redirectToLogged logger loc = do
  logger $ "redirecting to: " `mappend` TE.decodeUtf8 loc
  return $ redirectTo loc
