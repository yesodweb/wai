{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

main = run 3000 app

app req = ($
    case rawPathInfo req of
        "/builder/withlen" -> builderWithLen
        "/builder/nolen" -> builderNoLen
        "/file/withlen" -> fileWithLen
        "/file/nolen" -> fileNoLen
        "/source/withlen" -> sourceWithLen
        "/source/nolen" -> sourceNoLen
        "/notfound" -> responseFile status200 [] "notfound" Nothing
        x -> index x)

builderWithLen = responseBuilder
    status200
    [ ("Content-Type", "text/plain")
    , ("Content-Length", "4")
    ]
    $ copyByteString "PONG"

builderNoLen = responseBuilder
    status200
    [ ("Content-Type", "text/plain")
    ]
    $ copyByteString "PONG"

sourceWithLen = responseStream
    status200
    [ ("Content-Type", "text/plain")
    , ("Content-Length", "4")
    ]
    $ \send _ -> send $ copyByteString "PONG"

sourceNoLen = responseStream
    status200
    [ ("Content-Type", "text/plain")
    ]
    $ \send _ -> send $ copyByteString "PONG"

fileWithLen = responseFile
    status200
    [ ("Content-Type", "text/plain")
    , ("Content-Length", "4")
    ]
    "pong.txt"
    Nothing

fileNoLen = responseFile
    status200
    [ ("Content-Type", "text/plain")
    ]
    "pong.txt"
    Nothing

index p = responseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p><a href='/builder/withlen'>builder withlen</a></p>\n"
    , "<p><a href='/builder/nolen'>builder nolen</a></p>\n"
    , "<p><a href='/file/withlen'>file withlen</a></p>\n"
    , "<p><a href='/file/nolen'>file nolen</a></p>\n"
    , "<p><a href='/source/withlen'>source withlen</a></p>\n"
    , "<p><a href='/source/nolen'>source nolen</a></p>\n"
    , p
    ]
