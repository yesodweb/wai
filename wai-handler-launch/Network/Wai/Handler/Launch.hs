{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Network.Wai.Handler.Launch
    ( run
    , runUrl
    , runUrlPort
    ) where

import Network.Wai
import Network.Wai.Internal
import Network.HTTP.Types
import qualified Network.Wai.Handler.Warp as Warp
import Data.IORef
import Data.Monoid (mappend)
import Control.Concurrent (forkIO, threadDelay)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import Control.Exception (throwIO)
import Data.Function (fix)
import qualified Data.ByteString as S
import Blaze.ByteString.Builder (fromByteString, Builder, flush)
import qualified Blaze.ByteString.Builder as Blaze
#if WINDOWS
import Foreign
import Foreign.C.String
#else
import System.Process (rawSystem)
#endif
import Data.Streaming.Blaze (newBlazeRecv, defaultStrategy)
import qualified Data.Streaming.Zlib as Z

ping :: IORef Bool -> Middleware
ping  var app req sendResponse
    | pathInfo req == ["_ping"] = do
        liftIO $ writeIORef var True
        sendResponse $ responseLBS status200 [] ""
    | otherwise = app req $ \res -> do
        let isHtml hs =
                case lookup "content-type" hs of
                    Just ct -> "text/html" `S.isPrefixOf` ct
                    Nothing -> False
        if isHtml $ responseHeaders res
            then do
                let (s, hs, withBody) = responseToStream res
                    (isEnc, headers') = fixHeaders id hs
                    headers'' = filter (\(x, _) -> x /= "content-length") headers'
                withBody $ \body ->
                    sendResponse $ responseStream s headers'' $ \sendChunk flush ->
                        addInsideHead sendChunk flush $ \sendChunk' flush' ->
                            if isEnc
                                then decode sendChunk' flush' body
                                else body sendChunk' flush'
            else sendResponse res

decode :: (Builder -> IO ()) -> IO ()
       -> StreamingBody
       -> IO ()
decode sendInner flushInner streamingBody = do
    (blazeRecv, blazeFinish) <- newBlazeRecv defaultStrategy
    inflate <- Z.initInflate $ Z.WindowBits 31
    let send builder = blazeRecv builder >>= goBuilderPopper
        goBuilderPopper popper = fix $ \loop -> do
            bs <- popper
            unless (S.null bs) $ do
                Z.feedInflate inflate bs >>= goZlibPopper
                loop
        goZlibPopper popper = fix $ \loop -> do
            res <- popper
            case res of
                Z.PRDone -> return ()
                Z.PRNext bs -> do
                    sendInner $ fromByteString bs
                    loop
                Z.PRError e -> throwIO e
    streamingBody send (send flush)
    mbs <- blazeFinish
    case mbs of
        Nothing -> return ()
        Just bs -> Z.feedInflate inflate bs >>= goZlibPopper
    Z.finishInflate inflate >>= sendInner . fromByteString

toInsert :: S.ByteString
toInsert = "<script>setInterval(function(){var x;if(window.XMLHttpRequest){x=new XMLHttpRequest();}else{x=new ActiveXObject(\"Microsoft.XMLHTTP\");}x.open(\"GET\",\"/_ping\",false);x.send();},60000)</script>"

addInsideHead :: (Builder -> IO ())
              -> IO ()
              -> StreamingBody
              -> IO ()
addInsideHead sendInner flushInner streamingBody = do
    (blazeRecv, blazeFinish) <- newBlazeRecv defaultStrategy
    ref <- newIORef $ Just (S.empty, whole)
    streamingBody (inner blazeRecv ref) (flush blazeRecv ref)
    state <- readIORef ref
    mbs <- blazeFinish
    held <- case mbs of
        Nothing -> return state
        Just bs -> push state bs
    case state of
        Nothing -> return ()
        Just (held, _) -> sendInner $ fromByteString held `mappend` fromByteString toInsert
  where
    whole = "<head>"

    flush blazeRecv ref = inner blazeRecv ref Blaze.flush

    inner blazeRecv ref builder = do
        state0 <- readIORef ref
        popper <- blazeRecv builder
        let loop state = do
                bs <- popper
                if S.null bs
                    then writeIORef ref state
                    else push state bs >>= loop
        loop state0

    push Nothing x = sendInner (fromByteString x) >> return Nothing
    push (Just (held, atFront)) x
        | atFront `S.isPrefixOf` x = do
            let y = S.drop (S.length atFront) x
            sendInner $ fromByteString held
              `mappend` fromByteString atFront
              `mappend` fromByteString toInsert
              `mappend` fromByteString y
            return Nothing
        | whole `S.isInfixOf` x = do
            let (before, rest) = S.breakSubstring whole x
            let after = S.drop (S.length whole) rest
            sendInner $ fromByteString held
              `mappend` fromByteString before
              `mappend` fromByteString whole
              `mappend` fromByteString toInsert
              `mappend` fromByteString after
            return Nothing
        | x `S.isPrefixOf` atFront = do
            let held' = held `S.append` x
                atFront' = S.drop (S.length x) atFront
            return $ Just (held', atFront')
        | otherwise = do
            let (held', atFront', x') = getOverlap whole x
            sendInner $ fromByteString held `mappend` fromByteString x'
            return $ Just (held', atFront')

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
    _ <- forkIO $ Warp.runSettings
        ( Warp.setPort port
        $ Warp.setOnException (\_ _ -> return ())
        $ Warp.setHost "*4" Warp.defaultSettings)
        $ ping x app
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
