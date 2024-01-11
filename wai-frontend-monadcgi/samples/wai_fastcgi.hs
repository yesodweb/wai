import qualified Network.CGI
import qualified Network.Wai.Frontend.MonadCGI
import qualified Network.Wai.Handler.FastCGI

main =
    Network.Wai.Handler.FastCGI.run $
        Network.Wai.Frontend.MonadCGI.cgiToApp mainCGI

mainCGI = Network.CGI.output "This is a test"
