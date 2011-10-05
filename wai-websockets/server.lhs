This is the Haskell implementation of the example for the WebSockets library. We
implement a simple multi-user chat program.

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE TemplateHaskell #-}
> import Data.Char (isPunctuation, isSpace)
> import Data.Monoid (mappend)
> import Data.Text (Text)
> import Control.Monad (forM_)
> import Control.Concurrent (MVar, newMVar, modifyMVar_, readMVar)
> import Control.Monad.IO.Class (liftIO)
> import qualified Data.Text as T
> import qualified Data.Text.IO as T

> import qualified Network.WebSockets as WS
> import qualified Network.Wai.Handler.Warp as Warp
> import qualified Network.Wai.Handler.WebSockets as WaiWS
> import qualified Network.Wai.Application.Static as Static
> import Data.FileEmbed (embedDir)

We represent a client by his username and a 'WS.Sender'. We can use this sender
to asynchronously send 'Text' to the client later.

> type Client = (Text, WS.Sender Text)

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
>     forM_ clients $ \(_, sender) -> sender WS.textData message

The main function first creates a new state for the server, then spawns the
actual server. For this purpose, we use the simple server provided by
'WS.runServer'.

> main :: IO ()
> main = do
>     putStrLn "http://localhost:9160/client.html"
>     state <- newMVar newServerState
>     Warp.runSettings Warp.defaultSettings
>       { Warp.settingsPort = 9160
>       , Warp.settingsIntercept = WaiWS.intercept (webSocketsApp state)
>       } staticApp

> staticApp = Static.staticApp Static.defaultFileServerSettings
>   { Static.ssFolder = Static.embeddedLookup $ Static.toEmbedded $(embedDir "static")
>   }

> webSocketsApp state = do

When a client connects, we first obtain the request. Once we have that, we use
the 'WS.handshake' function to generate a response, which we finally send to the
client.

We use irrefutable pattern matches here because the server will spawn a thread
for each connection -- and a crashing thread does not effect the rest of the
program. For a real application, proper logging is appropriate.

>         Just rq <- WS.receiveRequest
>         let Right rsp = WS.handshake rq
>         WS.sendResponse rsp

If we want to be able to send data to this client later, from another thread, we
obtain a sender. We will add this to the server state later.

>         sender <- WS.getSender

When a client is succesfully connected, we read the first message. This should
be in the format of "Hi, I am Jasper", where Jasper is the requested username.

>         msg <- WS.receiveData
>         clients <- liftIO $ readMVar state
>         case msg of
>             Nothing -> return ()
>             Just m

Check that the first message has the right format

>                 | not (prefix `T.isPrefixOf` m) ->
>                     WS.sendTextData ("Wrong announcement" :: Text)

Check the validity of the username

>                 | any ($ fst client)
>                     [T.null, T.any isPunctuation, T.any isSpace] ->
>                         WS.sendTextData ("Name cannot " `mappend`
>                             "contain punctuation or whitespace, and " `mappend`
>                             "cannot be empty" :: Text)

Check that the given username is not already taken

>                 | clientExists client clients ->
>                     WS.sendTextData ("User already exists" :: Text)

All is right!

>                 | otherwise -> do

We send a "Welcome!", according to our own little protocol. We add the client to
the list and broadcast the fact that he has joined. Then, we give control to the
'talk' function.

>                     liftIO $ modifyMVar_ state $ \s -> do
>                         let s' = addClient client s
>                         sender WS.textData $ "Welcome! Users: " `mappend`
>                             T.intercalate ", " (map fst s)
>                         broadcast (fst client `mappend` " joined") s'
>                         return s'
>                     talk state client
>               where
>                 prefix = "Hi! I am "
>                 client = (T.drop (T.length prefix) m, sender)

The talk function continues to read messages from a single client until he
disconnects. All messages are broadcasted to the other clients.

> talk :: MVar ServerState -> Client -> WS.WebSockets ()
> talk state client@(user, _) = do
>     msg <- WS.receiveData
>     case msg of
>         Nothing -> liftIO $ modifyMVar_ state $ \s -> do
>             let s' = removeClient client s
>             broadcast (user `mappend` " disconnected") s'
>             return s'
>         Just m -> do
>             liftIO $ readMVar state >>= broadcast
>                 (user `mappend` ": " `mappend` m)
>             talk state client
