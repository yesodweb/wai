import Control.Concurrent
import System.IO
import Network.Socket as N

main :: IO ()
main = do
    addr:_ <- N.getAddrInfo (Just N.defaultHints) (Just "127.0.0.1") (Just "3000")
    s <- N.openSocket addr
    N.connect s (addrAddress addr)
    putStrLn "Client connected"
    hdl <- N.socketToHandle s ReadWriteMode
    hPutStr hdl $ unlines
        [ "GET / HTTP/1.1"
        , ""
        , ""
        , ""
        ]
    threadDelay (100*1000)
    putStrLn "Client closing"
    N.close s
