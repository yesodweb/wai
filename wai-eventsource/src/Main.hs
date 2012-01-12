{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Network.Wai.Handler.Warp (run)
import Network.Wai
import Network.HTTP.Types (statusOK)
import Data.Time.Clock.POSIX
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

import Network.Wai.EventSource

app :: Chan ServerEvent -> Application
app chan req =
    case pathInfo req of
        []     -> return $ ResponseFile statusOK [("Content-Type", "text/html")] "static/index.html" Nothing
        ["es"] -> eventSourceApp chan req
        _ -> error "unexpected pathInfo"


source :: Chan ServerEvent -> IO ()
source chan = forever $ do
    threadDelay 1000000
    time <- getPOSIXTime
    writeChan chan (ServerEvent Nothing Nothing [fromString . show $ time])


main :: IO ()
main = do
    chan <- newChan
    _ <- forkIO . source $ chan
    run 8000 (app chan)
