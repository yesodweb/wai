import qualified Network.CGI
import qualified Network.Wai.Handler.FastCGI
import qualified Network.Wai.Frontend.MonadCGI

main = Network.Wai.Handler.FastCGI.run
     $ Network.Wai.Frontend.MonadCGI.cgiToApp mainCGI

mainCGI = Network.CGI.output "This is a test"
