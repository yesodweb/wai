
module Network.Wai.Handler.Warp.WithApplication (
  withApplication,
  openFreePort,
) where

import           Control.Concurrent
import           Control.Exception
import           Network.Socket
import           Network.Wai
import           Network.Wai.Handler.Warp.Run
import           Network.Wai.Handler.Warp.Settings
import           Network.Wai.Handler.Warp.Types

data App
  = App {
    appThread :: ThreadId,
    appKilled :: Waiter (),
    appExceptionMVar :: MVar (Maybe SomeException),
    appPort :: Int
  }

-- | Runs the given 'Application' on a free port. Passes the port to the given
-- operation and executes it, while the 'Application' is running. Shuts down the
-- server before returning.
--
-- Handy for e.g. testing 'Application's over a real network port.
withApplication :: IO Application -> (Port -> IO a) -> IO a
withApplication mkApp action = do
  app <- mkApp
  bracket (acquire app) free (\ runningApp -> action (appPort runningApp))
  where
    acquire :: Application -> IO App
    acquire app = do
      start <- mkWaiter
      killed <- mkWaiter
      exceptionMVar_ <- newMVar Nothing
      thread <- forkIO $ do
        (port, sock) <- openFreePort
        let settings =
              defaultSettings{
                settingsBeforeMainLoop = notify start port
              }
        runSettingsSocket settings sock (handleApp exceptionMVar_ app)
          `finally` notify killed ()
      port <- waitFor start
      return $ App thread killed exceptionMVar_ port

    free :: App -> IO ()
    free runningApp = do
      killThread $ appThread runningApp
      waitFor $ appKilled runningApp
      exception <- readMVar (appExceptionMVar runningApp)
      case exception of
        Nothing -> return ()
        Just e -> throwIO e

handleApp :: MVar (Maybe SomeException) -> Application -> Application
handleApp mvar app request respond = do
  catch (app request respond) $ \ e -> do
    modifyMVar_ mvar $ \ _ ->
      return (Just e)
    throwIO e

data Waiter a
  = Waiter {
    notify :: a -> IO (),
    waitFor :: IO a
  }

mkWaiter :: IO (Waiter a)
mkWaiter = do
  mvar <- newEmptyMVar
  return $ Waiter {
    notify = putMVar mvar,
    waitFor = readMVar mvar
  }

-- | Opens a socket on a free port and returns both port and socket.
openFreePort :: IO (Port, Socket)
openFreePort = do
  s <- socket AF_INET Stream defaultProtocol
  localhost <- inet_addr "127.0.0.1"
  bind s (SockAddrInet aNY_PORT localhost)
  listen s 1
  port <- socketPort s
  return (fromIntegral port, s)
