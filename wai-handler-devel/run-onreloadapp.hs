
import Network.Wai.Handler.DevelServer (runQuitWithReloadActions)
import System.Directory(findExecutable)
import System.Process(readProcess)

main :: IO ()
main = runQuitWithReloadActions 4000 "SmallApp" "smallApp" (const $ return []) [browserRefresh] 

browserRefresh :: IO (IO ())
browserRefresh = do
  exe <- findExecutable "xdotool"
  case exe of 
    Nothing -> do
      putStrLn "Install xdotool for automatic browser refresh"
      return $ return () 
    Just xdotool -> do
      putStrLn "Please click on the browser window..."
      pid <- readProcess xdotool ["selectwindow"] ""
      return $ do
        current <- readProcess xdotool ["getwindowfocus"] ""
        let cmds = ["windowfocus", pid, "key", "F5", "windowfocus", current]
        _ <- readProcess xdotool  cmds ""
        return ()
