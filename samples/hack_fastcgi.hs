import qualified Network.CGI
import qualified Hack.Handler.FastCGI
import qualified Hack.Frontend.MonadCGI

main = Hack.Handler.FastCGI.runFastCGIorCGI
     $ Hack.Frontend.MonadCGI.cgiToApp mainCGI

mainCGI = Network.CGI.output "This is a test"
