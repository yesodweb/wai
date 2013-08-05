{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromChunks)
import Data.Text ()
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Middleware.Gzip
import Network.Wai.Middleware.Jsonp
import Network.Wai.Handler.Warp

app :: Application
app request = return $ case pathInfo request of
    [] -> responseLBS status200 []
            $ fromChunks $ flip map [1..10000] $ \i -> pack $ concat
                    [ "<p>Just this same paragraph again. "
                    , show (i :: Int)
                    , "</p>"
                    ]
    ["test.html"] -> ResponseFile status200 [] "test.html" Nothing
    ["json"]      -> ResponseFile status200 [(hContentType, "application/json")]
                                               "json" Nothing
    _             -> ResponseFile status404 [] "../LICENSE" Nothing

main :: IO ()
main = run 3000 $ gzip def $ jsonp app
