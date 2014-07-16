websockets example
==================

This is the Haskell implementation of the example for the WebSockets library. We
implement a simple multi-user chat program. A live demo of the example is
available [here](http://jaspervdj.be/websockets-example). In order to understand
this example, keep the [reference](http://jaspervdj.be/websockets/reference)
nearby to check out the functions we use.

> {-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
> import Data.Char (isPunctuation, isSpace)
> import Data.Monoid (mappend)
> import Data.Text (Text)
> import Control.Exception (fromException, handle)
> import Control.Monad (forM_, forever)
> import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar, forkIO)
> import Control.Monad.IO.Class (liftIO)
> import qualified Data.Text as T
> import qualified Data.Text.IO as T

> import qualified Network.WebSockets as WS
> import qualified Network.Wai
> import qualified Network.Wai.Handler.Warp as Warp
> import qualified Network.Wai.Handler.WebSockets as WaiWS
> import qualified Network.Wai.Application.Static as Static
> import Data.FileEmbed (embedDir)

We represent a client by his username and a 'WS.Connection'. We will see how we obtain this 'WS.Connection' later on.

> type Client = (Text, WS.Connection)

The state kept on the server is simply a list of connected clients. We've added
an alias and some utility functions, so it will be easier to extend this state
later on.

> type ServerState = [Client]

Create a new, initial state

> newServerState :: ServerState
> newServerState = []

Get the number of active clients

> numClients :: ServerState -> Int
> numClients = length

Check if a user already exists (based on username)

> clientExists :: Client -> ServerState -> Bool
> clientExists client = any ((== fst client) . fst)

Add a client (first, you should verify the client is not already connected using
'clientExists')

> addClient :: Client -> ServerState -> ServerState
> addClient client clients = client : clients

Remove a client

> removeClient :: Client -> ServerState -> ServerState
> removeClient client = filter ((/= fst client) . fst)

Send a message to all clients, and log it on stdout.

> broadcast :: Text -> ServerState -> IO ()
> broadcast message clients = do
>     T.putStrLn message
>     forM_ clients $ \(_, conn) -> forkIO $ WS.sendTextData conn message

The main function first creates a new state for the server, then spawns the
actual server. For this purpose, we use the simple server provided by
'WS.runServer'.

> main :: IO ()
> main = do
>     putStrLn "http://localhost:9160/client.html"
>     state <- newMVar newServerState
>     Warp.runSettings Warp.defaultSettings
>       { Warp.settingsPort = 9160
>       } $ WaiWS.websocketsOr WS.defaultConnectionOptions (application state) staticApp

> staticApp :: Network.Wai.Application
> staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

When a client connects, we accept the connection, regardless of the path.

> application :: MVar ServerState -> WS.ServerApp
> application state pending = do
>     conn <- WS.acceptRequest pending

When a client is succesfully connected, we read the first message. This should
be in the format of "Hi, I am Jasper", where Jasper is the requested username.

>     msg <- WS.receiveData conn
>     clients <- liftIO $ readMVar state
>     case msg of

Check that the first message has the right format

>         _   | not (prefix `T.isPrefixOf` msg) ->
>                 WS.sendTextData conn ("Wrong announcement" :: Text)

Check the validity of the username

>             | any ($ fst client)
>                 [T.null, T.any isPunctuation, T.any isSpace] ->
>                     WS.sendTextData conn ("Name cannot " `mappend`
>                         "contain punctuation or whitespace, and " `mappend`
>                         "cannot be empty" :: Text)

Check that the given username is not already taken

>             | clientExists client clients ->
>                 WS.sendTextData conn ("User already exists" :: Text)

All is right!

>             | otherwise -> do

We send a "Welcome!", according to our own little protocol. We add the client to
the list and broadcast the fact that he has joined. Then, we give control to the
'talk' function.

>                liftIO $ modifyMVar_ state $ \s -> do
>                    let s' = addClient client s
>                    WS.sendTextData conn $
>                        "Welcome! Users: " `mappend`
>                        T.intercalate ", " (map fst s)
>                    broadcast (fst client `mappend` " joined") s'
>                    return s'
>                talk conn state client
>           where
>             prefix = "Hi! I am "
>             client = (T.drop (T.length prefix) msg, conn)

The talk function continues to read messages from a single client until he
disconnects. All messages are broadcasted to the other clients.

> talk :: WS.Connection -> MVar ServerState -> Client -> IO ()
> talk conn state client@(user, _) = handle catchDisconnect $
>     forever $ do 
>         msg <- WS.receiveData conn
>         liftIO $ readMVar state >>= broadcast
>             (user `mappend` ": " `mappend` msg)
>   where
>     catchDisconnect e = case fromException e of
>         Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
>             let s' = removeClient client s
>             broadcast (user `mappend` " disconnected") s'
>             return s'
>         _ -> return ()
