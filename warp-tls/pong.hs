{-# LANGUAGE OverloadedStrings #-}

import Blaze.ByteString.Builder (copyByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Data.Monoid
import Network.HTTP.Conduit
import Network.HTTP.ReverseProxy
import Network.HTTP.Types (status200)
import Network.Wai
import Network.Wai.Handler.Warp (defaultSettings, setPort)
import Network.Wai.Handler.WarpTLS

main = do
    putStrLn "https://localhost:3009/"
    -- manager <- newManager def
    runTLS
        (tlsSettings "certificate.pem" "key.pem")
            { onInsecure = AllowInsecure
            }
        (setPort 3009 defaultSettings)
        app

app req f = f $
    case rawPathInfo req of
        "/builder/withlen" -> builderWithLen
        "/builder/nolen" -> builderNoLen
        "/file/withlen" -> fileWithLen
        "/file/nolen" -> fileNoLen
        "/source/withlen" -> sourceWithLen
        "/source/nolen" -> sourceNoLen
        "/secure" -> responseLBS status200 [] $ if isSecure req then "secure" else "insecure"
        x -> index x

builderWithLen =
    responseBuilder
        status200
        [ ("Content-Type", "text/plain")
        , ("Content-Length", "4")
        ]
        $ copyByteString "PONG"

builderNoLen =
    responseBuilder
        status200
        [ ("Content-Type", "text/plain")
        ]
        $ copyByteString "PONG"

sourceWithLen = responseStream
    status200
    [ ("Content-Type", "text/plain")
    , ("Content-Length", "4")
    ]
    $ \sendChunk _flush -> sendChunk $ copyByteString "PONG"

sourceNoLen = responseStream
    status200
    [ ("Content-Type", "text/plain")
    ]
    $ \sendChunk _flush -> sendChunk $ copyByteString "PONG"

fileWithLen =
    responseFile
        status200
        [ ("Content-Type", "text/plain")
        , ("Content-Length", "4")
        ]
        "pong.txt"
        Nothing

fileNoLen =
    responseFile
        status200
        [ ("Content-Type", "text/plain")
        ]
        "pong.txt"
        Nothing

index p =
    responseBuilder status200 [("Content-Type", "text/html")] $
        mconcat $
            map
                copyByteString
                [ "<p><a href='/builder/withlen'>builder withlen</a></p>\n"
                , "<p><a href='/builder/nolen'>builder nolen</a></p>\n"
                , "<p><a href='/file/withlen'>file withlen</a></p>\n"
                , "<p><a href='/file/nolen'>file nolen</a></p>\n"
                , "<p><a href='/source/withlen'>source withlen</a></p>\n"
                , "<p><a href='/source/nolen'>source nolen</a></p>\n"
                , p
                ]
