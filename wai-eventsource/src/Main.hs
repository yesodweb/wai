{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.Trans (liftIO)
import Control.Concurrent.Chan
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Gzip (gzip, def)
import Network.Wai
import Network.HTTP.Types (statusOK)
import Data.Time.Clock.POSIX
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import qualified Data.Conduit as C

import Network.Wai.EventSource

app :: Chan ServerEvent -> Application
app chan req =
    case pathInfo req of
        []     -> return $ ResponseFile statusOK [("Content-Type", "text/html")] "static/index.html" Nothing
        ["esold"]  -> eventSourceApp chan req
        ["eschan"] -> eventSourceAppChan chan req
        ["esio"]   -> eventSourceAppIO eventIO req
        ["essrc"]  -> eventSourceAppSource eventSource req
        _ -> error "unexpected pathInfo"

eventChan :: Chan ServerEvent -> IO ()
eventChan chan = forever $ do
    threadDelay 1000000
    time <- getPOSIXTime
    writeChan chan (ServerEvent Nothing Nothing [fromString . show $ time])

eventIO :: IO ServerEvent
eventIO = do
    threadDelay 1000000
    time <- getPOSIXTime
    return $ ServerEvent (Just $ fromString "io")
                         Nothing
                         [fromString . show $ time]

eventSource :: C.Source IO ServerEvent
eventSource = C.sourceState () (const pull)
  where
    pull = do
      time <- liftIO $ do
        threadDelay 1000000
        getPOSIXTime
      return $ C.StateOpen () $ ServerEvent (Just $ fromString "source")
                                            Nothing
                                            [fromString . show $ time]

main = do
    chan <- newChan
    _ <- forkIO . eventChan $ chan
    run 8000 (gzip def $ app chan)
