{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (copyByteString)
import Data.Monoid
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL

main = run 3000 app

app req = return $
    case rawPathInfo req of
        "/builder/withlen" -> builderWithLen
        "/builder/nolen" -> builderNoLen
        "/file/withlen" -> fileWithLen
        "/file/nolen" -> fileNoLen
        "/source/withlen" -> sourceWithLen
        "/source/nolen" -> sourceNoLen
        "/notfound" -> ResponseFile status200 [] "notfound" Nothing
        x -> index x

builderWithLen = ResponseBuilder
    status200
    [ ("Content-Type", "text/plain")
    , ("Content-Length", "4")
    ]
    $ copyByteString "PONG"

builderNoLen = ResponseBuilder
    status200
    [ ("Content-Type", "text/plain")
    ]
    $ copyByteString "PONG"

sourceWithLen = ResponseSource
    status200
    [ ("Content-Type", "text/plain")
    , ("Content-Length", "4")
    ]
    $ CL.sourceList [C.Chunk $ copyByteString "PONG"]

sourceNoLen = ResponseSource
    status200
    [ ("Content-Type", "text/plain")
    ]
    $ CL.sourceList [C.Chunk $ copyByteString "PONG"]

fileWithLen = ResponseFile
    status200
    [ ("Content-Type", "text/plain")
    , ("Content-Length", "4")
    ]
    "pong.txt"
    Nothing

fileNoLen = ResponseFile
    status200
    [ ("Content-Type", "text/plain")
    ]
    "pong.txt"
    Nothing

index p = ResponseBuilder status200 [("Content-Type", "text/html")] $ mconcat $ map copyByteString
    [ "<p><a href='/builder/withlen'>builder withlen</a></p>\n"
    , "<p><a href='/builder/nolen'>builder nolen</a></p>\n"
    , "<p><a href='/file/withlen'>file withlen</a></p>\n"
    , "<p><a href='/file/nolen'>file nolen</a></p>\n"
    , "<p><a href='/source/withlen'>source withlen</a></p>\n"
    , "<p><a href='/source/nolen'>source nolen</a></p>\n"
    , p
    ]
