{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Webkit
import Network.HTTP.Types

main :: IO ()
main = run "Sample App" app

app :: Application
app _ = return $ responseLBS status200 [("Content-Type", "text/html")] "<h1>Hello World!</h1>"
