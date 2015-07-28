{-# LANGUAGE CPP #-}
module Network.Wai.Middleware.CleanPath
    ( cleanPath
    ) where

import Network.Wai
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Types (status301, hLocation)
import Data.Text (Text)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid (mconcat)
#endif

cleanPath :: ([Text] -> Either B.ByteString [Text])
          -> B.ByteString
          -> ([Text] -> Application)
          -> Application
cleanPath splitter prefix app env sendResponse =
    case splitter $ pathInfo env of
        Right pieces -> app pieces env sendResponse
        Left p -> sendResponse
                $ responseLBS status301
                  [(hLocation, mconcat [prefix, p, suffix])]
                $ L.empty
    where
        -- include the query string if present
        suffix =
            case B.uncons $ rawQueryString env of
                Nothing -> B.empty
                Just ('?', _) -> rawQueryString env
                _ -> B.cons '?' $ rawQueryString env
