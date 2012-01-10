{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Network.Wai.Handler.Warp (run)
import Network.Wai
import Network.HTTP.Types (statusOK)
import Data.Enumerator (run_, ($$))
import Data.Enumerator.List (generateM)
import Data.Time.Clock.POSIX
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

import EventStream


app :: Chan ServerEvent -> Application
app chan req =
    case pathInfo req of
        []     -> return $ ResponseFile statusOK [("Content-Type", "text/html")] "static/index.html" Nothing
        ["es"] -> do
            chan' <- liftIO $ dupChan chan
            return $ res chan'
        _ -> error "unexpected pathInfo"


res :: Chan ServerEvent -> Response
res chan = ResponseEnumerator (resE chan)


resE :: Chan ServerEvent -> ResponseEnumerator a
resE chan genIter =
    run_ $ generateM (fmap eventToBuilder $ readChan chan) $$ iter
  where
    iter = genIter statusOK [("Content-Type", "text/event-stream")]


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
