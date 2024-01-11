{-# LANGUAGE OverloadedStrings #-}

import Blaze.ByteString.Builder (copyByteString)
import Control.Concurrent
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Monoid
import Network
import Network.HTTP.Types (status200)
import Network.Socket (Socket (..), SocketStatus (..), mkSocket)
import Network.Wai
import Network.Wai.Handler.Warp (defaultSettings)
import Network.Wai.Handler.WarpTLS
import System.Environment (getEnv)
import System.Posix.Process
import System.Posix.Signals
import UnliftIO.Async

envSocketName = "GRACEFUL_PONG_SOCKET"

fromSocket2Env sock = [(envSocketName, show (fd, addrFamily, socketType, protocolNumber))]
  where
    MkSocket fd addrFamily socketType protocolNumber _socketStatus = sock

handleSIGHUP proc spawn =
    modifyMVar_ proc $ \oldpid -> do
        pid <- spawn
        signalProcess lostConnection oldpid
        status <- waitpid oldpid
        print (oldpid, status)
        return pid

waitpid pid = do
    mstatus <- getProcessStatus False True pid
    case mstatus of
        Nothing -> waitpid pid
        Just status -> return status

handleSIGTERM proc finish =
    takeMVar proc
        >>= signalProcess softwareTermination
        >> putMVar finish ()

child (fd, addrFamily, socketType, protocolNumber) = do
    sock <- mkSocket fd addrFamily socketType protocolNumber Listening
    putStrLn "https://localhost:3000/"
    finish <- newEmptyMVar
    let sighup = Catch $ putMVar finish ()
    _handler <- installHandler lostConnection sighup Nothing
    runTLSSocket (TLSSettings "certificate.pem" "key.pem") defaultSettings sock app
        `race_` takeMVar finish

parent = do
    sock <- listenOn $ PortNumber $ toEnum 3000
    let env = Just $ fromSocket2Env sock
        spawn = forkProcess $ executeFile "./graceful-pong" False [] env
    proc <- spawn >>= newMVar
    finish <- newEmptyMVar
    let sighup = Catch $ handleSIGHUP proc spawn
        sigterm = Catch $ handleSIGTERM proc finish
    _handler <- installHandler lostConnection sighup Nothing
    _handler <- installHandler softwareTermination sigterm Nothing
    takeMVar finish

main = do
    (getEnv envSocketName >>= child . read) `catch` const parent

app req = return $
    case rawPathInfo req of
        "/builder/withlen" -> builderWithLen
        "/builder/nolen" -> builderNoLen
        "/file/withlen" -> fileWithLen
        "/file/nolen" -> fileNoLen
        "/source/withlen" -> sourceWithLen
        "/source/nolen" -> sourceNoLen
        x -> index x

builderWithLen =
    ResponseBuilder
        status200
        [ ("Content-Type", "text/plain")
        , ("Content-Length", "4")
        ]
        $ copyByteString "PONG"

builderNoLen =
    ResponseBuilder
        status200
        [ ("Content-Type", "text/plain")
        ]
        $ copyByteString "PONG"

sourceWithLen =
    ResponseSource
        status200
        [ ("Content-Type", "text/plain")
        , ("Content-Length", "4")
        ]
        $ CL.sourceList [C.Chunk $ copyByteString "PONG"]

sourceNoLen =
    ResponseSource
        status200
        [ ("Content-Type", "text/plain")
        ]
        $ CL.sourceList [C.Chunk $ copyByteString "PONG"]

fileWithLen =
    ResponseFile
        status200
        [ ("Content-Type", "text/plain")
        , ("Content-Length", "4")
        ]
        "pong.txt"
        Nothing

fileNoLen =
    ResponseFile
        status200
        [ ("Content-Type", "text/plain")
        ]
        "pong.txt"
        Nothing

index p =
    ResponseBuilder status200 [("Content-Type", "text/html")] $
        mconcat $
            map
                copyByteString
                [ "<p><a href='/builder/withlen'>builder withlen</a></p>\n"
                , "<p><a href='/builder/nolen'>builder nolen</a></p>\n"
                , "<p><a href='/file/withlen'>file withlen</a></p>\n"
                , "<p><a href='/file/nolen'>file nolen</a></p>\n"
                , "<p><a href='/source/withlen'>source withlen</a></p>\n"
                , "<p><a href='/source/nolen'>source nolen</a></p>\n"
                , p
                ]
