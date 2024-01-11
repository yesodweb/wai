{-# LANGUAGE CPP #-}

module Network.Wai.Middleware.Vhost (
    vhost,
    redirectWWW,
    redirectTo,
    redirectToLogged,
) where

import qualified Data.ByteString as BS
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mappend)
#endif
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import Network.HTTP.Types as H
import Network.Wai

vhost :: [(Request -> Bool, Application)] -> Application -> Application
vhost vhosts def req =
    case filter (\(b, _) -> b req) vhosts of
        [] -> def req
        (_, app) : _ -> app req

redirectWWW :: Text -> Application -> Application -- W.MiddleWare
redirectWWW home =
    redirectIf
        home
        (maybe True (BS.isPrefixOf "www") . lookup "host" . requestHeaders)

redirectIf :: Text -> (Request -> Bool) -> Application -> Application
redirectIf home cond app req sendResponse =
    if cond req
        then sendResponse $ redirectTo $ TE.encodeUtf8 home
        else app req sendResponse

redirectTo :: BS.ByteString -> Response
redirectTo location =
    responseLBS
        H.status301
        [(H.hContentType, "text/plain"), (H.hLocation, location)]
        "Redirect"

redirectToLogged :: (Text -> IO ()) -> BS.ByteString -> IO Response
redirectToLogged logger loc = do
    logger $ "redirecting to: " `mappend` TE.decodeUtf8 loc
    return $ redirectTo loc
