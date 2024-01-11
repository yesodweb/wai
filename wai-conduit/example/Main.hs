#!/usr/bin/env stack
-- stack --resolver lts-11.10 script

import Conduit
import Control.Monad.Trans.Resource
import Data.ByteString.Builder (byteString)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Conduit
import Network.Wai.Handler.Warp

main :: IO ()
main = run 3000 app

app :: Application
app _req respond =
    runResourceT $ withInternalState $ \is ->
        respond $
            responseSource status200 [] $
                transPipe (`runInternalState` is) (sourceFile "Main.hs")
                    .| mapC (Chunk . byteString)
