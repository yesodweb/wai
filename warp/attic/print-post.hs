{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.Wai.Handler.Warp
import Network.HTTP.Types (status200)
import Blaze.ByteString.Builder (fromByteString)
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)

{-
 - use `curl -H "Transfer-Encoding: chunked" estfile http://localhost:3000/` to send a chunked post request.
 -}

main = run 3000 app

app req = do
    (requestBody req C.$$ CL.consume) >>= liftIO . print
    return $ ResponseBuilder status200 [] (fromByteString "PONG")
