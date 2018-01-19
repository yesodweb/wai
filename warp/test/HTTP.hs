module HTTP (
    sendGET
  , sendGETwH
  , sendHEAD
  , sendHEADwH
  , responseBody
  , responseStatus
  , responseHeaders
  , getHeaderValue
  , HeaderName
  ) where

import Network.HTTP.Client
import Network.HTTP.Types
import Data.ByteString
import qualified Data.ByteString.Lazy as BL

sendGET :: String -> IO (Response BL.ByteString)
sendGET url = sendGETwH url []

sendGETwH :: String -> [Header] -> IO (Response BL.ByteString)
sendGETwH url hdr = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest url
    let request' = request { requestHeaders = hdr }
    response <- httpLbs request' manager
    return response

sendHEAD :: String -> IO (Response BL.ByteString)
sendHEAD url = sendHEADwH url []

sendHEADwH :: String -> [Header] -> IO (Response BL.ByteString)
sendHEADwH url hdr = do
    manager <- newManager defaultManagerSettings
    request <- parseRequest url
    let request' = request { requestHeaders = hdr, method = methodHead }
    response <- httpLbs request' manager
    return response

getHeaderValue :: HeaderName -> [Header] -> Maybe ByteString
getHeaderValue = lookup
