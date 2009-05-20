import qualified Network.CGI
import qualified Hack.Handler.CGI
import qualified Hack.Frontend.MonadCGI

main = Hack.Handler.CGI.run $ Hack.Frontend.MonadCGI.cgiToApp mainCGI

mainCGI = Network.CGI.output "This is a test"
