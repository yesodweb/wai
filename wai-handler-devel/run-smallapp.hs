import Network.Wai.Handler.DevelServer (runQuit)

main :: IO ()
main = do
  putStrLn "http://localhost:3000"
  runQuit 3000 "SmallApp" "smallApp" (const $ return [])
