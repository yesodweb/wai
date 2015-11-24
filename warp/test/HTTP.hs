{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HTTP (
    sendGET
  , sendGETwH
  , sendHEAD
  , sendHEADwH
  , rspBody
  , rspCode
  , rspHeaders
  , getHeaderValue
  , HeaderName(..)
  , mkHeader
  ) where

import Network.HTTP
import Network.Stream

sendGET :: String -> IO (Response String)
sendGET url = sendGETwH url []

sendGETwH :: String -> [Header] -> IO (Response String)
sendGETwH url hdr = unResult $ simpleHTTP $ (getRequest url) { rqHeaders = hdr }

sendHEAD :: String -> IO (Response String)
sendHEAD url = sendHEADwH url []

sendHEADwH :: String -> [Header] -> IO (Response String)
sendHEADwH url hdr = unResult $ simpleHTTP $ (headRequest url) { rqHeaders = hdr }

unResult :: IO (Result (Response String)) -> IO (Response String)
unResult action = do
    res <- action
    case res of
        Right rsp -> return rsp
        Left _ -> error "Connection error"

getHeaderValue :: HasHeaders a => HeaderName -> a -> Maybe String
getHeaderValue key r = case retrieveHeaders key r of
    []  -> Nothing
    x:_ -> Just $ hdrValue x

deriving instance Eq Header
