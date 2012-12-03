{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module FullApp (fullApp) where

import Network.Wai
import Network.HTTP.Types
import Data.ByteString.Lazy.Char8 (pack)
import Database.Persist.TH
import Database.Persist.Sqlite
import System.Directory
import Control.Monad (when)
import Text.Hamlet
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Data.Text.Lazy.Encoding (encodeUtf16LE)
import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.IO.Class (liftIO)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persist|
Dummy
    dummy String
|]

fullApp :: (Application -> IO ()) -> IO ()
fullApp handler = do
    putStrLn "testApp called, this should happen only once per reload"
    -- Swap between the following two lines as necessary to generate errors
    exi <- doesFileExist "db"
    --let exi = True
    when exi $ removeFile "db"
    withSqlitePool "db" 10 $ \pool -> do
        flip runSqlPool pool $ runMigration migrateAll
        handler $ \req -> do
            if pathInfo req == ["favicon.ico"]
                then return $ responseLBS status301 [("Location", "http://www.yesodweb.com/favicon.ico")]
                            $ pack ""
                else do
                    liftIO $ print (pathInfo req)
                    x <- runResourceT $ flip runSqlPool pool $ do
                        _ <- insert (Dummy "")
                        count ([] :: [Filter Dummy])
                    return $ responseLBS status200
                        [("Content-Type", "text/html; charset=utf-8")] $
                        encodeUtf16LE $ renderHtml
                        $(shamletFile "hamlet/testapp.hamlet")
        putStrLn "handler completed, this should only happen at the beginning of a reload"
