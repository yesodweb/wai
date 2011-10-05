{-# LANGUAGE OverloadedStrings #-}
module AMQPListener
    (
      AMQPEvent(..)
    , openEventChannel
    ) where

import           Control.Applicative((<$>), (<*>))
import           Control.Monad(mzero)
import           Control.Monad.Fix(fix)
import           Control.Concurrent(forkIO)
import           Control.Concurrent.Chan(Chan, newChan, readChan, writeChan)

import           Data.Aeson(FromJSON(..), Value(..), Result(..), fromJSON, json, (.:), (.:?))
import           Data.Attoparsec(parse, maybeResult)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as LB

import           Data.Maybe(fromJust, fromMaybe)
import           Data.String.Utils(split)

import           Text.URI(URI(..), parseURI)
import           System.Posix.Env(getEnvDefault)
import           Network.AMQP

-- |The AMQPEvent represents and incomming message that should be
-- mapped to an EventSource event.
data AMQPEvent = AMQPEvent
    { amqpChannel :: B.ByteString
    , amqpData :: B.ByteString
    , amqpId :: Maybe B.ByteString
    , amqpName :: Maybe B.ByteString 
    }

instance FromJSON AMQPEvent where
    parseJSON (Object v) = AMQPEvent <$>
                           v .: "channel" <*>
                           v .: "data" <*>
                           v .:? "id" <*>
                           v .:? "name"
    parseJSON _           = mzero

-- |Connects to an AMQP broker.
-- Tries to get credentials, host and vhost from the AMQP_URL
-- environment variable
-- Take an exchange name and a queue name
openEventChannel :: String -> String -> IO (Chan AMQPEvent)
openEventChannel exchange queue = do
    amqpURI <- getEnvDefault "AMQP_URL" "amqp://guest:guest@127.0.0.1/"

    let uri   = fromJust $ parseURI amqpURI
    let auth  = fromMaybe "guest:guest" $ uriUserInfo uri
    let host  = fromMaybe "127.0.0.1"   $ uriRegName uri
    let vhost = uriPath uri

    let [user,password] = split ":" auth

    conn <- openConnection host vhost user password
    chan <- openChannel conn

    declareQueue chan newQueue {queueName = queue, queueAutoDelete = True, queueDurable = False}
    declareExchange chan newExchange {exchangeName = exchange, exchangeType = "fanout", exchangeDurable = False}
    bindQueue chan queue exchange queue

    listener <- newChan
    forkIO $ fix $ \loop -> readChan listener >> loop
    consumeMsgs chan queue NoAck (sendTo listener)
    return listener

-- |Write messages from AMQP to a channel
sendTo :: Chan AMQPEvent -> (Message, Envelope) -> IO ()
sendTo chan (msg, _) =
    case maybeResult $ parse json (B.concat $ LB.toChunks (msgBody msg)) of
        Just value -> case fromJSON value of
            Success event -> do
                writeChan chan event
            Error _       -> do
                return ()
        Nothing    -> return ()
