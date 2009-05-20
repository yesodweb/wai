module Hack.Frontend.MonadCGI
    ( cgiToApp
    ) where

import Hack
import Network.CGI.Monad
import Network.CGI.Protocol

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS

safeRead :: Read a => a -> String -> a
safeRead d s = case reads s of
                ((x, _):_) -> x
                _ -> d

cgiToApp :: CGI CGIResult -> Application
cgiToApp cgi env = do
    let vars = http env
        input = hackInput env
        (inputs, body') = decodeInput vars input
        req = CGIRequest
                { cgiVars = Map.fromList vars
                , cgiInputs = inputs
                , cgiRequestBody = body'
                }
    (headers'', output') <- runCGIT cgi req
    let output = case output' of
                    CGIOutput bs -> bs
                    CGINothing -> BS.empty
    let headers' = map (\(HeaderName x, y) -> (x, y)) headers''
    let status' = case lookup "Status" headers' of
                    Nothing -> 200
                    Just s -> safeRead 200 s
    return $ Response status' headers' output
