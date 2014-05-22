{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Launch
import Network.Wai.Middleware.Gzip

main = runUrl "FIXME" $ gzip def $ \_ f -> f $ responseLBS status200 [("Content-Type", "text/html; charset=utf8")] "<html><head></head><body>HELLO THERE"
