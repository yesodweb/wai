module Network.Wai.Frontend.MonadCGI
    ( cgiToApp
    , cgiToAppGeneric
    ) where

import Network.Wai
import Network.CGI.Monad
import Network.CGI.Protocol
import Network.HTTP.Types (Status (..))
import Control.Monad.IO.Class (liftIO)
import Data.CaseInsensitive (original)

import qualified Data.Map as Map
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Char8 as S8

import Control.Arrow (first)
import Data.Char (toUpper)
import Data.String (fromString)

safeRead :: Read a => a -> String -> a
safeRead d s = case reads s of
                ((x, _):_) -> x
                _ -> d

cgiToApp :: CGI CGIResult -> Application
cgiToApp = cgiToAppGeneric id

cgiToAppGeneric :: Monad m
                => (m (Headers, CGIResult) -> IO (Headers, CGIResult))
                -> CGIT m CGIResult
                -> Application
cgiToAppGeneric toIO cgi env sendResponse = do
    input <- lazyRequestBody env
    let vars = map (first fixVarName . go) (requestHeaders env)
               ++ getCgiVars env
        (inputs, body') = decodeInput vars input
        req = CGIRequest
                { cgiVars = Map.fromList $ vars
                , cgiInputs = inputs
                , cgiRequestBody = body'
                }
    (headers'', output') <- liftIO $ toIO $ runCGIT cgi req
    let output = case output' of
                    CGIOutput bs -> bs
                    CGINothing -> BS.empty
    let headers' = map (\(HeaderName x, y) ->
                         (fromString x, S8.pack y)) headers''
    let status' = case lookup (fromString "Status") headers' of
                    Nothing -> 200
                    Just s -> safeRead 200 $ S8.unpack s
    sendResponse $ responseLBS (Status status' S8.empty) headers' output
  where
    go (x, y) = (S8.unpack $ original x, S8.unpack y)

fixVarName :: String -> String
fixVarName = ((++) $ "HTTP_") . map fixVarNameChar

fixVarNameChar :: Char -> Char
fixVarNameChar '-' = '_'
fixVarNameChar c = toUpper c

getCgiVars :: Request -> [(String, String)]
getCgiVars e =
    [ ("PATH_INFO", S8.unpack $ rawPathInfo e)
    , ("REQUEST_METHOD", show $ requestMethod e)
    , ("QUERY_STRING", S8.unpack $ rawQueryString e)
    ]
