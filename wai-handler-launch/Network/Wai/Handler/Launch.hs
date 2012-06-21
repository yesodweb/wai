{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Network.Wai.Handler.Launch
    ( run
    , runUrl
    , runUrlPort
    ) where

import Network.Wai
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as Warp
import Data.IORef
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as S
import Blaze.ByteString.Builder (fromByteString)
#if WINDOWS
import Foreign
import Foreign.String
#else
import System.Cmd (rawSystem)
#endif
import Data.Conduit.Zlib (decompressFlush, WindowBits (WindowBits))
import Data.Conduit.Blaze (builderToByteStringFlush)
import Data.Conduit
import qualified Data.Conduit.List as CL

ping :: IORef Bool -> Middleware
ping  var app req
    | pathInfo req == ["_ping"] = do
        liftIO $ writeIORef var True
        return $ responseLBS status200 [] ""
    | otherwise = do
        res <- app req
        let isHtml hs =
                case lookup "content-type" hs of
                    Just ct -> "text/html" `S.isPrefixOf` ct
                    Nothing -> False
        case res of
            ResponseFile _ hs _ _
                | not $ isHtml hs -> return res
            ResponseBuilder _ hs _
                | not $ isHtml hs -> return res
            ResponseSource _ hs _
                | not $ isHtml hs -> return res
            _ -> do
                let (s, hs, body) = responseSource res
                let (isEnc, headers') = fixHeaders id hs
                let headers'' = filter (\(x, _) -> x /= "content-length") headers'
                let fixEnc src =
                        if isEnc then
                            src $= decompressFlush (WindowBits 31)
                            else src
                return $ ResponseSource s headers''
                    $ fixEnc (body $= builderToByteStringFlush)
                    $= insideHead
                    $= CL.map (fmap fromByteString)

toInsert :: S.ByteString
toInsert = "<script>setInterval(function(){var x;if(window.XMLHttpRequest){x=new XMLHttpRequest();}else{x=new ActiveXObject(\"Microsoft.XMLHTTP\");}x.open(\"GET\",\"/_ping\",false);x.send();},60000)</script>"

insideHead :: Pipe l (Flush S.ByteString) (Flush S.ByteString) r (ResourceT IO) r
insideHead =
    loop' (S.empty, whole)
  where
    loop' state = awaitE >>= either (close state) (push' state)
    whole = "<head>"
    push' state (Chunk x) = push state x
    push' state Flush = yield Flush >> loop' state

    push (held, atFront) x
        | atFront `S.isPrefixOf` x = do
            let y = S.drop (S.length atFront) x
            mapM_ (yield . Chunk) [held, atFront, toInsert, y]
            CL.map id
        | whole `S.isInfixOf` x = do
            let (before, rest) = S.breakSubstring whole x
            let after = S.drop (S.length whole) rest
            mapM_ (yield . Chunk) [held, before, whole, toInsert, after]
            CL.map id
        | x `S.isPrefixOf` atFront = do
            let held' = held `S.append` x
                atFront' = S.drop (S.length x) atFront
            loop' (held', atFront')
        | otherwise = do
            let (held', atFront', x') = getOverlap whole x
            mapM_ (yield . Chunk) [held, x']
            loop' (held', atFront')

    close (held, _) r = mapM_ yield [Chunk held, Chunk toInsert] >> return r

getOverlap :: S.ByteString -> S.ByteString -> (S.ByteString, S.ByteString, S.ByteString)
getOverlap whole x =
    go whole
  where
    go piece
        | S.null piece = ("", whole, x)
        | piece `S.isSuffixOf` x =
            let x' = S.take (S.length x - S.length piece) x
                atFront = S.drop (S.length piece) whole
             in (piece, atFront, x')
        | otherwise = go $ S.init piece

fixHeaders :: ([Header] -> [Header])
           -> [Header]
           -> (Bool, [Header])
fixHeaders front [] = (False, front [])
fixHeaders front (("content-encoding", "gzip"):rest) = (True, front rest)
fixHeaders front (x:xs) = fixHeaders (front . (:) x) xs

#if WINDOWS
foreign import ccall "launch"
    launch' :: Int -> CString -> IO ()
#endif

launch :: Int -> String -> IO ()

#if WINDOWS
launch port s = withCString s $ launch' port
#else
launch port s = forkIO (rawSystem
#if MAC
    "open"
#else
    "xdg-open"
#endif
    ["http://127.0.0.1:" ++ show port ++ "/" ++ s] >> return ()) >> return ()
#endif

run :: Application -> IO ()
run = runUrl ""

runUrl :: String -> Application -> IO ()
runUrl = runUrlPort 4587

runUrlPort :: Int -> String -> Application -> IO ()
runUrlPort port url app = do
    x <- newIORef True
    _ <- forkIO $ Warp.runSettings Warp.defaultSettings
        { Warp.settingsPort = port
        , Warp.settingsOnException = const $ return ()
        , Warp.settingsHost = "127.0.0.1"
        } $ ping x app
    launch port url
    loop x

loop :: IORef Bool -> IO ()
loop x = do
    let seconds = 120
    threadDelay $ 1000000 * seconds
    b <- readIORef x
    if b
        then writeIORef x False >> loop x
        else return ()
