WAI: Web Application Interface
==============================

Getting started
---------------

You want a minimal example? Here it is!

~~~ {.haskell}
{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

app :: Application
app _ respond = do
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app
~~~

Put that code into a file named _hello.hs_ and install [wai] and [warp] from Hackage:

    cabal install wai warp

Run it:

    runhaskell hello.hs

Point your browser to:

    http://localhost:8080/


Serving static content
----------------------

We can modify our previous example to serve static content. For this create a file named _index.html_:

    <p>Hello, Web!</p>

Now we redefine `responseBody` to refer to that file:

~~~ {.haskell}
app2 :: Application
app2 _ respond = respond index

index :: Response
index = responseFile
    status200
    [("Content-Type", "text/html")]
    "index.html"
    Nothing
~~~


Basic dispatching
-----------------

An `Application` maps `Request`s to `Response`s:

    ghci> :info  Application
    type Application = Request -> IO Response

Depending on the path info provided with each `Request` we can serve different `Response`s:

~~~ {.haskell}
app3 :: Application
app3 request respond = respond $ case rawPathInfo request of
    "/"     -> index
    "/raw/" -> plainIndex
    _       -> notFound

plainIndex :: Response
plainIndex = responseFile
    status200
    [("Content-Type", "text/plain")]
    "index.html"
    Nothing

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"
~~~


Doing without overloaded strings
--------------------------------

For the sake of efficiency, WAI uses the [bytestring] package.  We used GHCs [overloaded strings] to almost hide this fact. But we can easily do without.  What follows is a more verbose definition of `notFound`, that works without GHC extensions:

~~~ {.haskell .ignore}
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy.Char8 as LB8
import           Data.CaseInsensitive (mk)

notFound = responseLBS
    status404
    [(mk $ B8.pack "Content-Type", B8.pack "text/plain")]
    (LB8.pack "404 - Not Found")
~~~


 [wai]: http://hackage.haskell.org/package/wai
 [warp]: http://hackage.haskell.org/package/warp
 [overloaded strings]: http://www.haskell.org/ghc/docs/latest/html/users_guide/type-class-extensions.html#overloaded-strings
 [bytestring]: http://hackage.haskell.org/package/bytestring
