import qualified Network.CGI
import qualified Network.Wai.Frontend.MonadCGI
import qualified Network.Wai.Handler.SimpleServer

main =
    Network.Wai.Handler.SimpleServer.run 3000 $
        Network.Wai.Frontend.MonadCGI.cgiToApp mainCGI

mainCGI = Network.CGI.output "This is a test"
