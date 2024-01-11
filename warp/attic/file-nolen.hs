{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

main =
    run 3000 $
        const $
            return $
                ResponseFile status200 [("Content-Type", "text/plain")] "test.txt" Nothing
