import Network.Wai.Handler.DevelServer (runQuit)

main :: IO ()
main = runQuit 3000 "SmallApp" "smallApp" (const $ return [])
