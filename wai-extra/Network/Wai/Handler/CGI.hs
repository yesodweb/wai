{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}
-- | Backend for Common Gateway Interface. Almost all users should use the
-- 'run' function.
module Network.Wai.Handler.CGI
    ( run
    , runSendfile
    , runGeneric
    , requestBodyFunc
    ) where

import Network.Wai
import Network.Wai.Internal
import Network.Socket (getAddrInfo, addrAddress)
import Data.IORef
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import Control.Arrow ((***))
import Data.Char (toLower)
import qualified System.IO
import qualified Data.String as String
import Data.Monoid (mconcat, mempty)
import Blaze.ByteString.Builder (fromByteString, toLazyByteString, flush)
import Blaze.ByteString.Builder.Char8 (fromChar, fromString)
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import System.IO (Handle)
import Network.HTTP.Types (Status (..))
import qualified Network.HTTP.Types as H
import qualified Data.CaseInsensitive as CI
import Data.Monoid (mappend)
import qualified Data.Streaming.Blaze as Blaze
import Data.Function (fix)
import Control.Monad (unless, void)

#if WINDOWS
import System.Environment (getEnvironment)
#else
import qualified System.Posix.Env.ByteString as Env

getEnvironment :: IO [(String, String)]
getEnvironment = map (B.unpack *** B.unpack) `fmap` Env.getEnvironment
#endif

safeRead :: Read a => a -> String -> a
safeRead d s =
  case reads s of
    ((x, _):_) -> x
    [] -> d

lookup' :: String -> [(String, String)] -> String
lookup' key pairs = fromMaybe "" $ lookup key pairs

-- | Run an application using CGI.
run :: Application -> IO ()
run app = do
    vars <- getEnvironment
    let input = requestBodyHandle System.IO.stdin
        output = B.hPut System.IO.stdout
    runGeneric vars input output Nothing app

-- | Some web servers provide an optimization for sending files via a sendfile
-- system call via a special header. To use this feature, provide that header
-- name here.
runSendfile :: B.ByteString -- ^ sendfile header
            -> Application -> IO ()
runSendfile sf app = do
    vars <- getEnvironment
    let input = requestBodyHandle System.IO.stdin
        output = B.hPut System.IO.stdout
    runGeneric vars input output (Just sf) app

-- | A generic CGI helper, which allows other backends (FastCGI and SCGI) to
-- use the same code as CGI. Most users will not need this function, and can
-- stick with 'run' or 'runSendfile'.
runGeneric
     :: [(String, String)] -- ^ all variables
     -> (Int -> IO (IO B.ByteString)) -- ^ responseBody of input
     -> (B.ByteString -> IO ()) -- ^ destination for output
     -> Maybe B.ByteString -- ^ does the server support the X-Sendfile header?
     -> Application
     -> IO ()
runGeneric vars inputH outputH xsendfile app = do
    let rmethod = B.pack $ lookup' "REQUEST_METHOD" vars
        pinfo = lookup' "PATH_INFO" vars
        qstring = lookup' "QUERY_STRING" vars
        contentLength = safeRead 0 $ lookup' "CONTENT_LENGTH" vars
        remoteHost' =
            case lookup "REMOTE_ADDR" vars of
                Just x -> x
                Nothing ->
                    case lookup "REMOTE_HOST" vars of
                        Just x -> x
                        Nothing -> ""
        isSecure' =
            case map toLower $ lookup' "SERVER_PROTOCOL" vars of
                "https" -> True
                _ -> False
    addrs <- getAddrInfo Nothing (Just remoteHost') Nothing
    requestBody' <- inputH contentLength
    let addr =
            case addrs of
                a:_ -> addrAddress a
                [] -> error $ "Invalid REMOTE_ADDR or REMOTE_HOST: " ++ remoteHost'
        reqHeaders = map (cleanupVarName *** B.pack) vars
        env = Request
            { requestMethod = rmethod
            , rawPathInfo = B.pack pinfo
            , pathInfo = H.decodePathSegments $ B.pack pinfo
            , rawQueryString = B.pack qstring
            , queryString = H.parseQuery $ B.pack qstring
            , requestHeaders = reqHeaders
            , isSecure = isSecure'
            , remoteHost = addr
            , httpVersion = H.http11 -- FIXME
            , requestBody = requestBody'
            , vault = mempty
            , requestBodyLength = KnownLength $ fromIntegral contentLength
            , requestHeaderHost = lookup "host" reqHeaders
            , requestHeaderRange = lookup "range" reqHeaders
            }
    void $ app env $ \res ->
        case (xsendfile, res) of
            (Just sf, ResponseFile s hs fp Nothing) -> do
                mapM_ outputH $ L.toChunks $ toLazyByteString $ sfBuilder s hs sf fp
                return ResponseReceived
            _ -> do
                let (s, hs, wb) = responseToStream res
                (blazeRecv, blazeFinish) <- Blaze.newBlazeRecv Blaze.defaultStrategy
                wb $ \b -> do
                    let sendBuilder builder = do
                            popper <- blazeRecv builder
                            fix $ \loop -> do
                                bs <- popper
                                unless (B.null bs) $ do
                                    outputH bs
                                    loop
                    sendBuilder $ headers s hs `mappend` fromChar '\n'
                    b sendBuilder (sendBuilder flush)
                blazeFinish >>= maybe (return ()) outputH
                return ResponseReceived
  where
    headers s hs = mconcat (map header $ status s : map header' (fixHeaders hs))
    status (Status i m) = (fromByteString "Status", mconcat
        [ fromString $ show i
        , fromChar ' '
        , fromByteString m
        ])
    header' (x, y) = (fromByteString $ CI.original x, fromByteString y)
    header (x, y) = mconcat
        [ x
        , fromByteString ": "
        , y
        , fromChar '\n'
        ]
    sfBuilder s hs sf fp = mconcat
        [ headers s hs
        , header $ (fromByteString sf, fromString fp)
        , fromChar '\n'
        , fromByteString sf
        , fromByteString " not supported"
        ]
    fixHeaders h =
        case lookup "content-type" h of
            Nothing -> ("Content-Type", "text/html; charset=utf-8") : h
            Just _ -> h

cleanupVarName :: String -> CI.CI B.ByteString
cleanupVarName "CONTENT_TYPE" = "Content-Type"
cleanupVarName "CONTENT_LENGTH" = "Content-Length"
cleanupVarName "SCRIPT_NAME" = "CGI-Script-Name"
cleanupVarName s =
    case s of
        'H':'T':'T':'P':'_':a:as -> String.fromString $ a : helper' as
        _ -> String.fromString s -- FIXME remove?
  where
    helper' ('_':x:rest) = '-' : x : helper' rest
    helper' (x:rest) = toLower x : helper' rest
    helper' [] = []

requestBodyHandle :: Handle -> Int -> IO (IO B.ByteString)
requestBodyHandle h = requestBodyFunc $ \i -> do
    bs <- B.hGet h i
    return $ if B.null bs then Nothing else Just bs

requestBodyFunc :: (Int -> IO (Maybe B.ByteString)) -> Int -> IO (IO B.ByteString)
requestBodyFunc get count0 = do
    ref <- newIORef count0
    return $ do
        count <- readIORef ref
        if count <= 0
            then return B.empty
            else do
                mbs <- get $ min count defaultChunkSize
                writeIORef ref $ count - maybe 0 B.length mbs
                return $ fromMaybe B.empty mbs
