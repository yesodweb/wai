{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Wai.Handler.Warp.HTTP2.PushPromise where

import qualified Network.HTTP.Types as H
import qualified Network.HTTP2.Server as H2
import qualified UnliftIO

import Network.Wai
import Network.Wai.Handler.Warp.FileInfoCache
import Network.Wai.Handler.Warp.HTTP2.Request (getHTTP2Data)
import Network.Wai.Handler.Warp.HTTP2.Types
import Network.Wai.Handler.Warp.Imports
import Network.Wai.Handler.Warp.Types

fromPushPromises :: InternalInfo -> Request -> IO [H2.PushPromise]
fromPushPromises ii req = do
    mh2data <- getHTTP2Data req
    let pp = maybe [] http2dataPushPromise mh2data
    catMaybes <$> mapM (fromPushPromise ii) pp

fromPushPromise :: InternalInfo -> PushPromise -> IO (Maybe H2.PushPromise)
fromPushPromise ii (PushPromise path file rsphdr w) = do
    efinfo <- UnliftIO.tryIO $ getFileInfo ii file
    case efinfo of
        Left (_ex :: UnliftIO.IOException) -> return Nothing
        Right finfo -> do
            let !siz = fromIntegral $ fileInfoSize finfo
                !fileSpec = H2.FileSpec file 0 siz
                !rsp = H2.responseFile H.ok200 rsphdr fileSpec
                !pp = H2.pushPromise path rsp w
            return $ Just pp
