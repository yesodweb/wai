{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.ByteString.Builder (string8)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Control.Monad
import Data.Time.Clock.POSIX (getPOSIXTime)
import Network.HTTP.Types (status200)
import Network.Wai (Application, Middleware, pathInfo, responseFile)
import Network.Wai.EventSource (ServerEvent(..), eventSourceAppChan, eventSourceAppIO)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Gzip (gzip, def)


app :: Chan ServerEvent -> Application
app chan req respond =
    case pathInfo req of
        []     -> respond $ responseFile status200 [("Content-Type", "text/html")] "example/index.html" Nothing
        ["esold"]  -> eventSourceAppChan chan req respond
        ["eschan"] -> eventSourceAppChan chan req respond
        ["esio"]   -> eventSourceAppIO eventIO req respond
        _ -> error "unexpected pathInfo"

eventChan :: Chan ServerEvent -> IO ()
eventChan chan = forever $ do
    threadDelay 1000000
    time <- getPOSIXTime
    writeChan chan (ServerEvent Nothing Nothing [string8 . show $ time])

eventIO :: IO ServerEvent
eventIO = do
    threadDelay 1000000
    time <- getPOSIXTime
    return $ ServerEvent (Just $ string8 "io")
                         Nothing
                         [string8 . show $ time]

main :: IO ()
main = do
    chan <- newChan
    _ <- forkIO . eventChan $ chan
    run 8080 (gzip def $ headers $ app chan)
    where
      -- headers required for SSE to work through nginx
      -- not required if using warp directly
      headers :: Middleware
      headers = addHeaders [ ("X-Accel-Buffering", "no")
                           , ("Cache-Control", "no-cache")
                           ]
