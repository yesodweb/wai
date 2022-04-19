{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wai.Middleware.SelectSpec
  ( main,
    spec,
  )
where

import Data.ByteString (ByteString)
#if __GLASGOW_HASKELL__ < 804
import Data.Monoid ((<>))
#endif
import Network.HTTP.Types (Status, status200, status401, status500)
import Network.Wai
import Network.Wai.Middleware.Select
import Network.Wai.Test (SResponse (simpleStatus), request, runSession)
import Test.Hspec

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Select" $ do
  it "With empty select should passthrough" $
    runApp (selectMiddleware mempty) "/" `shouldReturn` status200
  it "With other path select should passthrough" $
    runApp (selectMiddleware $ selectOverride "/_" status401) "/" `shouldReturn` status200
  it "With path select should hit override" $
    runApp (selectMiddleware $ selectOverride "/_" status401) "/_" `shouldReturn` status401
  it "With twice path select should hit first override" $
    runApp (selectMiddleware $ selectOverride "/_" status401 <> selectOverride "/_" status500) "/_"
      `shouldReturn` status401
  it "With two paths select should hit first matching (first)" $
    runApp (selectMiddleware $ selectOverride "/_" status401 <> selectOverride "/-" status500) "/_"
      `shouldReturn` status401
  it "With two paths select should hit first matching (last)" $
    runApp (selectMiddleware $ selectOverride "/_" status401 <> selectOverride "/-" status500) "/-"
      `shouldReturn` status500
  it "With other two paths select should passthrough" $
    runApp (selectMiddleware $ selectOverride "/_" status401 <> selectOverride "/-" status500) "/"
      `shouldReturn` status200
  it "With mempty then the path select should hit the pass" $
    runApp (selectMiddleware $ mempty <> selectOverride "/-" status500) "/-"
      `shouldReturn` status500

runApp :: Middleware -> ByteString -> IO Status
runApp mw path =
  fmap simpleStatus $
    runSession
      (request $ defaultRequest {rawPathInfo = path})
      $ mw app

app :: Application
app = constApp status200

constApp :: Status -> Application
constApp status _ respond = respond $ responseLBS status [] ""

overrideMiddleware :: Application -> Middleware
overrideMiddleware = const

selectOverride :: ByteString -> Status -> MiddlewareSelection
selectOverride path status = selectMiddlewareOnRawPathInfo path $ overrideMiddleware $ constApp status
