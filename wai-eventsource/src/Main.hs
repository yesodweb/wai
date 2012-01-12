{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Control.Monad.Trans
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Chan
import Network.Wai.Handler.Warp (run)
import Network.Wai
import Network.HTTP.Types (statusOK)
import qualified Data.Conduit as C
import Data.Time.Clock.POSIX
import Blaze.ByteString.Builder.Char.Utf8 (fromString)
import Blaze.ByteString.Builder (Builder)

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
res chan = ResponseSource statusOK [("Content-Type", "text/event-stream")] (resE chan)


resE :: Chan ServerEvent -> C.Source IO Builder
resE chan =
  C.sourceIO ign (\_ -> ign) (\_ -> fmap (f . eventToBuilder) $ readChan chan)
  where
    ign = return ()
    f Nothing = C.Closed
    f (Just b) = C.Open b


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
