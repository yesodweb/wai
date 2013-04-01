{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
module Network.Wai.Handler.DevelServer
    ( run
    , runQuit
    , runNoWatch
    , runWithReloadActions
    , runQuitWithReloadActions
    ) where

import Language.Haskell.Interpreter hiding (typeOf)
import Network.Wai
import Network.HTTP.Types (status200)

import Data.Text.Lazy (pack)
import Data.Text.Lazy.Encoding (encodeUtf8)
import qualified Data.ByteString.Lazy.Char8 as L8
import Control.Exception (Exception, SomeException, toException, fromException, finally, mask)
import qualified Control.Exception as E
import Control.Concurrent (ThreadId, myThreadId, threadDelay, killThread)
import qualified Control.Concurrent as Concurrent

import Data.Maybe
import Control.Monad
import Control.Concurrent.MVar

import qualified Network.Wai.Handler.Warp as Warp
import Network.Wai.Application.Devel
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Data.List (nub, group, sort)

import Network.Wai.Handler.DevelServer.Compat

type FunctionName = String

runNoWatch :: Int -> ModuleName -> FunctionName
           -> (FilePath -> IO [FilePath]) -> IO ()
runNoWatch port modu func extras = do
    ah <- initAppHolder
    _ <- reload modu func extras Nothing ah []
    Warp.run port $ toApp ah

runQuit :: Int -> ModuleName -> FunctionName -> (FilePath -> IO [FilePath])
        -> IO ()
runQuit port modu func extras = runQuitWithReloadActions port modu func extras []

-- |
-- A version of `Concurrent.forkIO` that re-throws any uncaught
-- `E.UserInterrupt` from the child thread in the parent thread.
--
-- We need this because using hint (or the GHC API) causes @UserInterrupt@ to
-- occur in the thread that runs `runInterpreter` instead of the main thread.
forkIO :: IO () -> IO ThreadId
forkIO action = do
  threadId <- myThreadId
  Concurrent.forkIO (action `E.catch` \e -> case e of
    E.UserInterrupt -> E.throwTo threadId e
    _ -> E.throwIO e
    )

runQuitWithReloadActions :: Int -> ModuleName -> FunctionName -> (FilePath -> IO [FilePath])
                         -> [IO (IO ())] -> IO ()
runQuitWithReloadActions port modu func extras actions = do
    sig <- newEmptyMVar
    mask $ \unmask -> do
      threadId <- forkIO $ runWithReloadActions port modu func extras (Just sig) actions
      unmask (go sig) `finally` killThread threadId
  where
    go sig = do
        x <- getLine
        case x of
            'q':_ -> putStrLn "Quitting, goodbye!"
            'r':_ -> do
                putStrLn "Forcing reinterpretation"
                _ <- tryPutMVar sig ()
                go sig
            _ -> go sig

runWithReloadActions :: Int -> ModuleName -> FunctionName -> (FilePath -> IO [FilePath]) 
                     -> Maybe (MVar ()) -> [IO (IO ())] -> IO ()
runWithReloadActions port modu func extras msig initActions = do
    actions <- mapM id initActions
    ah <- initAppHolder
    mask $ \unmask -> do
      threadId <- forkIO $ fillApp modu func extras ah msig actions
      unmask (Warp.run port $ toApp ah) `finally` killThread threadId

run :: Int -> ModuleName -> FunctionName -> (FilePath -> IO [FilePath]) -> Maybe (MVar ())
    -> IO ()
run port modu func extras msig = runWithReloadActions port modu func extras msig []

{-
startApp :: Queue -> Handler -> IO ()
startApp queue withApp = do
    forkIO (withApp go) >> return ()
  where
    go app = do
        msession <- C.readChan queue
        case msession of
            Nothing -> return ()
            Just (req, onRes) -> do
                void $ forkIO $ (E.handle onErr $ app req) >>= onRes
                go app
    onErr :: SomeException -> IO Response
    onErr e = return
            $ responseLBS
                status500
                [("Content-Type", "text/plain; charset=utf-8")]
            $ charsToLBS
            $ "Exception thrown while running application\n\n" ++ show e
    void x = x >> return ()
-}

getTimes :: [FilePath] -> IO [TimeStamp]
getTimes = E.handle (constSE $ return []) . mapM getTimeStamp

constSE :: x -> SomeException -> x
constSE = const

fillApp :: String -> String
        -> (FilePath -> IO [FilePath]) -> AppHolder -> Maybe (MVar ()) -> [IO ()] -> IO ()
fillApp modu func dirs ah msig actions =
    go Nothing []
  where
    go prevError prevFiles = do
        forceReload <- maybe (return False) (fmap isJust . tryTakeMVar) msig
        toReload <-
            if forceReload || null prevFiles
                then return True
                else do
                    times <- getTimes $ map fst prevFiles
                    return $ times /= map snd prevFiles
        (newError, newFiles) <-
            if toReload
                then reload modu func dirs prevError ah actions
                else return (prevError, prevFiles)
        threadDelay 1000000
        go newError newFiles

reload :: String -> String
       -> (FilePath -> IO [FilePath])
       -> Maybe SomeException
       -> AppHolder
       -> [IO ()]
       -> IO (Maybe SomeException, [(FilePath, TimeStamp)])
reload modu func extras prevError ah actions = do
    case prevError of
         Nothing -> putStrLn "Attempting to interpret your app..."
         _       -> return ()
    loadingApp' prevError ah
    res <- theapp modu func
    case res of
        Left err -> do
            when (show (Just err) /= show prevError) $
               putStrLn $ "Compile failed: " ++ showInterpError err
            loadingApp' (Just $ toException err) ah
            return (Just $ toException err, [])
        Right (app, files') -> E.handle onInitErr $ do
            files'' <- mapM extras files'
            let files = map head $ group $ sort $ concat $ files' : files''
            putStrLn "Interpreting success, new app loaded"
            E.handle onInitErr $ do
                swapApp (\f -> app $ f . logStdoutDev) ah
                times <- getTimes files
                sequence_ actions
                return (Nothing, zip files times)
    where
        onInitErr e = do
            putStrLn $ "Error initializing application: " ++ show e
            loadingApp' (Just e) ah
            return (Just e, [])

showInterpError :: InterpreterError -> String
showInterpError (WontCompile errs) =
    concat . nub $ map (\(GhcError msg) -> '\n':'\n':msg) errs
showInterpError err = show err

loadingApp' :: Maybe SomeException -> AppHolder -> IO ()
loadingApp' err = swapApp (loadingApp err)

loadingApp :: Maybe SomeException -> Handler
loadingApp err f =
    f $ const $ return $ responseLBS status200
        ( ("Content-Type", "text/plain")
        : case err of
            Nothing -> [("Refresh", "1")]
            Just _ -> []
        ) $ toMessage err
  where
    toMessage Nothing = "Loading code changes, please wait"
    toMessage (Just err') = charsToLBS $ "Error loading code: " ++
        (case fromException err' of
            Just e -> showInterpError e
            Nothing -> show err')

charsToLBS :: String -> L8.ByteString
charsToLBS = encodeUtf8 . pack

type Handler = (Application -> IO ()) -> IO ()

theapp :: String -> String -> IO (Either InterpreterError (Handler, [FilePath]))
theapp modu func =
    runInterpreter $ do
        loadModules [modu]
        mods <- getLoadedModules
        setImports ["Prelude", "Network.Wai", "Data.ByteString.Internal", "Control.Monad.Trans.Resource", modu]
        app <- interpret func infer
        return (app, map toFile mods)
  where
    toFile s = map toSlash s ++ ".hs"
    toSlash '.' = '/'
    toSlash c   = c
