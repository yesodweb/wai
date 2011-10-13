{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Rewrite
    ( rewrite, autoHtmlRewrite
    ) where

import Network.Wai
import System.Directory (doesFileExist)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text, unpack, pack)
import qualified Data.Text.Encoding as TE
import qualified Data.Text as T
import Network.HTTP.Types as H


-- | rewrite based on your own conversion rules
-- Example usage: rewrite (autoHtmlRewrite "static")
rewrite :: ([Text] -> H.RequestHeaders -> IO [Text]) -> Middleware
rewrite convert app req = do
  newPathInfo <- liftIO $ convert (pathInfo req) (requestHeaders req)
  let rawPInfo = TE.encodeUtf8 $ T.intercalate "/" newPathInfo
  app req { pathInfo = newPathInfo, rawPathInfo =  rawPInfo }

-- | example rewriter
-- We don't recommend normally checking the file system on every request - this is just an example.
-- Implements 2 rules for static html re-writes
--   1) for a directory foo/, check for foo/index.html
--   2) for a non-directory bar, check for bar.html
-- Do the rewrite only if the html file exists.
autoHtmlRewrite :: String -> [Text] -> H.RequestHeaders -> IO [Text]
autoHtmlRewrite staticDir pieces' _ = do
    fe <- doesFileExist $ staticDir ++ "/" ++ reWritePath
    return $ if fe then map pack reWritePieces else pieces'
  where
    pieces = map unpack pieces'
    reWritePath = concat $ map ((:) '/') reWritePieces
    reWritePieces =
       if (null pieces) || (null $ last pieces)
          then pieces ++  ["index.html"]
          else (init pieces) ++ [(last pieces) ++ ".html"]
