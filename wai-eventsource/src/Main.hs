{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Network.Wai.Handler.Warp (run)
import Network.Wai
import Network.HTTP.Types (statusOK)
import Data.Enumerator (Iteratee, Step(Continue), Stream(Chunks,EOF), returnI, run_, (>>==), ($$), generateM)
import Data.Time.Clock.POSIX
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Blaze.ByteString.Builder (fromByteString)
import Blaze.ByteString.Builder.Char.Utf8 (fromString)

import EventStream


app :: Chan ServerEvent -> Application
app chan req =
    case pathInfo req of
        []     -> return $ ResponseFile statusOK [("Content-Type", "text/html")] "static/index.html" Nothing
        ["es"] -> do
            chan' <- liftIO $ dupChan chan
            return $ res chan'


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
    forkIO . source $ chan
    run 8000 (app chan)
