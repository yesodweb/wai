{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Rewrite
    ( rewrite, rewritePure
    ) where

import Network.Wai
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Network.HTTP.Types as H


-- | rewrite based on your own conversion rules
rewrite :: ([Text] -> H.RequestHeaders -> IO [Text]) -> Middleware
rewrite convert app req sendResponse = do
  newPathInfo <- liftIO $ convert (pathInfo req) (requestHeaders req)
  let rawPInfo = TE.encodeUtf8 $ T.intercalate "/" newPathInfo
  app req { pathInfo = newPathInfo, rawPathInfo =  rawPInfo } sendResponse

-- | rewrite based on your own conversion rules
-- Example convert function:

-- staticConvert :: [Text] -> H.RequestHeaders -> [Text]
-- staticConvert pieces _ = piecesConvert pieces
--   where
--    piecesConvert [] = ["static", "html", "pages.html"]
--    piecesConvert route@("pages":_) = "static":"html":route
rewritePure :: ([Text] -> H.RequestHeaders -> [Text]) -> Middleware
rewritePure convert app req =
  let pInfo = convert (pathInfo req) (requestHeaders req)
      rawPInfo = TE.encodeUtf8 $ T.intercalate "/" pInfo
  in  app req { pathInfo = pInfo, rawPathInfo =  rawPInfo }
