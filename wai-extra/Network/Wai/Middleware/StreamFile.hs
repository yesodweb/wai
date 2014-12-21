{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.StreamFile where

import Network.Wai
import Network.Wai.Internal
import Network.Wai (Middleware, responseToStream)
import Network.HTTP.Types (Status, ResponseHeaders)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString as S
import qualified Data.Streaming.Blaze as B
import qualified Blaze.ByteString.Builder as Blaze
import Control.Monad (unless)
import Data.Function (fix)
import System.Posix

streamFile :: Middleware
streamFile app env sendResponse = app env $ \res ->
    case res of
      ResponseFile _ _ fp _ -> withBody sendBody
          where
            (s, hs, withBody) = responseToStream res
            sendBody :: StreamingBody -> IO ResponseReceived
            sendBody body = do
               len <- getFileSize fp
               let hs' = ("Content-Length", (S8.pack (show len))) : hs
               sendResponse $ responseStream s hs' body
      _ -> sendResponse res

getFileSize :: FilePath -> IO FileOffset
getFileSize path = do
    stat <- getFileStatus path
    return (fileSize stat)
