module Hack.Frontend.MonadCGI
    ( cgiToApp
    ) where

import Hack
import Network.CGI.Monad
import Network.CGI.Protocol

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS

import Control.Arrow (first)
import Data.Char (toUpper)

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
                { cgiVars = Map.fromList $ map (first fixVarName) vars
                                        ++ getCgiVars env
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

fixVarName :: String -> String
fixVarName = (flip (++) $ "HTTP_") . map fixVarNameChar

fixVarNameChar :: Char -> Char
fixVarNameChar '-' = '_'
fixVarNameChar c = toUpper c

getCgiVars :: Env -> [(String, String)]
getCgiVars e =
    [ ("PATH_INFO", pathInfo e)
    , ("REQUEST_METHOD", show $ requestMethod e)
    , ("SCRIPT_NAME", scriptName e)
    , ("QUERY_STRING", queryString e)
    , ("SERVER_NAME", serverName e)
    , ("SERVER_PORT", show $ serverPort e)
    ]
