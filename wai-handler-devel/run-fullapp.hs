import Network.Wai.Handler.DevelServer (runQuit)

main :: IO ()
main = runQuit 3000 "FullApp" "fullApp" (const $ return ["hamlet/testapp.hamlet"])
