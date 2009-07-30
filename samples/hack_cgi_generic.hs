import qualified Network.CGI
import qualified Hack.Handler.CGI
import qualified Hack.Frontend.MonadCGI
import Control.Monad.Reader

main :: IO ()
main = Hack.Handler.CGI.run
     $ Hack.Frontend.MonadCGI.cgiToAppGeneric
       monadToIO
       mainCGI

mainCGI :: Network.CGI.CGIT (Reader String) Network.CGI.CGIResult
mainCGI = do
    s <- lift ask
    Network.CGI.output s

monadToIO :: Reader String a -> IO a
monadToIO = return . (flip runReader) "This is a generic test"
