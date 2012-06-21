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
> import Control.Exception (fromException)
> import Control.Monad (forM_)
> import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
> import Control.Monad.IO.Class (liftIO)
> import qualified Data.Text as T
> import qualified Data.Text.IO as T

> import qualified Network.WebSockets as WS
> import qualified Network.Wai
> import qualified Network.Wai.Handler.Warp as Warp
> import qualified Network.Wai.Handler.WebSockets as WaiWS
> import qualified Network.Wai.Application.Static as Static
> import Data.FileEmbed (embedDir)

We represent a client by his username and a 'WS.Sender'. We can use this sender
to asynchronously send 'Text' to the client later. Note that using `WS.Hybi00`
here does not imply that our server is only compatible with the `hybi-00`
version of the protocol, for more details on this, see the
[Network.WebSockets](http://jaspervdj.be/websockets/reference/Network-WebSockets.html) 
reference.

> type Client = (Text, WS.Sink WS.Hybi00)

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
>     forM_ clients $ \(_, sink) -> WS.sendSink sink $ WS.textData message

The main function first creates a new state for the server, then spawns the
actual server. For this purpose, we use the simple server provided by
'WS.runServer'.

> main :: IO ()
> main = do
>     putStrLn "http://localhost:9160/client.html"
>     state <- newMVar newServerState
>     Warp.runSettings Warp.defaultSettings
>       { Warp.settingsPort = 9160
>       , Warp.settingsIntercept = WaiWS.intercept (application state)
>       } staticApp

> staticApp :: Network.Wai.Application
> staticApp = Static.staticApp $ Static.embeddedSettings $(embedDir "static")

When a client connects, we accept the connection, regardless of the path.

> application :: MVar ServerState -> WS.Request -> WS.WebSockets WS.Hybi00 ()
> application state rq = do
>     WS.acceptRequest rq

We log some information here: in particular, we are interested in the protocol
version our client is using (for debugging purposes).

>     WS.getVersion >>= liftIO . putStrLn . ("Client version: " ++)

If we want to be able to send data to this client later, from another thread, we
obtain a sink. We will add this to the server state later.

>     sink <- WS.getSink

When a client is succesfully connected, we read the first message. This should
be in the format of "Hi, I am Jasper", where Jasper is the requested username.

>     msg <- WS.receiveData
>     clients <- liftIO $ readMVar state
>     case msg of

Check that the first message has the right format

>         _   | not (prefix `T.isPrefixOf` msg) ->
>                 WS.sendTextData ("Wrong announcement" :: Text)

Check the validity of the username

>             | any ($ fst client)
>                 [T.null, T.any isPunctuation, T.any isSpace] ->
>                     WS.sendTextData ("Name cannot " `mappend`
>                         "contain punctuation or whitespace, and " `mappend`
>                         "cannot be empty" :: Text)

Check that the given username is not already taken

>             | clientExists client clients ->
>                 WS.sendTextData ("User already exists" :: Text)

All is right!

>             | otherwise -> do

We send a "Welcome!", according to our own little protocol. We add the client to
the list and broadcast the fact that he has joined. Then, we give control to the
'talk' function.

>                liftIO $ modifyMVar_ state $ \s -> do
>                    let s' = addClient client s
>                    WS.sendSink sink $ WS.textData $
>                        "Welcome! Users: " `mappend`
>                        T.intercalate ", " (map fst s)
>                    broadcast (fst client `mappend` " joined") s'
>                    return s'
>                talk state client
>           where
>             prefix = "Hi! I am "
>             client = (T.drop (T.length prefix) msg, sink)

The talk function continues to read messages from a single client until he
disconnects. All messages are broadcasted to the other clients.

> talk :: WS.Protocol p => MVar ServerState -> Client -> WS.WebSockets p ()
> talk state client@(user, _) = flip WS.catchWsError catchDisconnect $ do
>     msg <- WS.receiveData
>     liftIO $ readMVar state >>= broadcast
>         (user `mappend` ": " `mappend` msg)
>     talk state client
>   where
>     catchDisconnect e = case fromException e of
>         Just WS.ConnectionClosed -> liftIO $ modifyMVar_ state $ \s -> do
>             let s' = removeClient client s
>             broadcast (user `mappend` " disconnected") s'
>             return s'
>         _ -> return ()
