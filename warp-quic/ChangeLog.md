# Revision history for warp-quic

## 0.0.5

* A QUIC server can be stopped through `settingsInstallShutdownHandler`, as a
  TCP one can, and tells its peers it is going instead of leaving them to wait
  out their idle timeout. Requires `quic` 0.3.15 or later; built against an
  earlier one, this is as it was. Not on Windows: `quic` waits on its socket
  the way warp does, so it needs the same Windows work warp has had, and that
  is not written yet.
  [#1123](https://github.com/yesodweb/wai/pull/1123)
* Stopping lets each HTTP/3 connection finish first: it is sent GOAWAY and
  answers the requests it has taken, and the server stops once they are done
  or `settingsGracefulShutdownTimeout` has passed.  It used to close every
  connection at once, cutting the requests in progress.  Requires `http3`
  0.1.10 or later; built against an earlier one, the connections are closed
  at once as before.

## 0.0.4

* Adjusted code to fit new range of dependency constraints. [#1043](https://github.com/yesodweb/wai/pull/1043)
    * Accept lower versions of `http3` and `tls`

## 0.0.3

* Using http3 v0.1.

## 0.0.2

* Labeling threads.
* New API: runQUICSockets.

## 0.0.1

* Providing `runQUICSocket`.

## 0.0.0

* First version. Released on an unsuspecting world.
