#!/usr/bin/env stack
-- stack --resolver lts-11.10 script
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Conduit
import Network.HTTP.Types
import Conduit
import Data.ByteString.Builder (byteString)
import Control.Monad.Trans.Resource

main :: IO ()
main = run 3000 app

app :: Application
app _req respond =
  runResourceT $ withInternalState $ \is ->
  respond $ responseSource status200 [] $
  transPipe (`runInternalState` is) (sourceFile "Main.hs") .|
  mapC (Chunk . byteString)
