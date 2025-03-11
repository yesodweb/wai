# ChangeLog

## 3.4.13

* Introduced new smart constructor `tlsSettingsSni` to make it more convenient
  to dynamically change certificates. Deprecates `tlsSettingsRef` and
  `tlsSettingsChainRef`.
  [#1025](https://github.com/yesodweb/wai/pull/1025)

## 3.4.12

* Rethrowing asynchronous exceptions
  [#1013](https://github.com/yesodweb/wai/pull/1013)

## 3.4.11

* Removing `unliftio`.

## 3.4.10

* Removed `data-default` dependency entirely. Does now require `>= tls-2.1.3`.
  [#1011](https://github.com/yesodweb/wai/pull/1011)

## 3.4.9

* Using `timeout` for `handshake` to prevent thread leaks.

## 3.4.8

* Substituted `data-default-class` for `data-default` [#1010](https://github.com/yesodweb/wai/pull/1010)

## 3.4.7

* Expose `attachConn` to use post-handshake TLS connection.
  [#1007](https://github.com/yesodweb/wai/pull/1007)

## 3.4.6

* Preparing for tls v2.1

## 3.4.5

* Making mkConn of WarpTLS interruptible
  [#984](https://github.com/yesodweb/wai/pull/984)

## 3.4.4

* Allow warp v3.4.

## 3.4.3

* Install shutdown handlers passed via `Settings` to `run...` functions

## 3.4.2

* Requiring warp v3.3.29.

## 3.4.1

* Supporting `tls` v1.8.0.

## 3.4.0

* Major version up to deprecate v3.3.7 due to the incompatibility
  against cryptonite.

## 3.3.7

* Using crypton instead of cryptonite.
  [#931](https://github.com/yesodweb/wai/pull/931)

## 3.3.6

* Setting FD_CLOEXEC on the listening socket.
  [#923](https://github.com/yesodweb/wai/pull/923)

## 3.3.5

* Switching the version of the "recv" package from 0.0.x to 0.1.x.

## 3.3.4

* Integrated customizable `accept` hook from `Network.Wai.Handler.Warp.Settings` (cf. `setAccept`)
  [#912](https://github.com/yesodweb/wai/pull/912)
* Adjusted `httpOverTls` because of the factoring out of `Network.Wai.Handler.Warp.Recv` to its own package `recv` in the `warp` package.
  [#899](https://github.com/yesodweb/wai/pull/899)

## 3.3.3

* Creating a bigger buffer when the current one is too small to fit the Builder
  [#895](https://github.com/yesodweb/wai/pull/895)
* Expose TLS.supportedHashSignatures via TLSSettings
  [#872](https://github.com/yesodweb/wai/pull/872)

## 3.3.2

* Providing the Internal module.
  [#841](https://github.com/yesodweb/wai/issues/841)

## 3.3.1

* Move exception handling over to `unliftio` for better async exception support [#845](https://github.com/yesodweb/wai/issues/845)
* Cleanly close connection when client closes connection prematurely [#844](https://github.com/yesodweb/wai/issues/844)

## 3.3.0

* Breaking changes: certFile and keyFile are not exported anymore.
* Allow TLS credentials to be retrieved from an IORef.
  [#806](https://github.com/yesodweb/wai/pull/806)

## 3.2.12

* A config field: tlsCredentials and tlsSessionManager.
  [#805](https://github.com/yesodweb/wai/pull/805)

## 3.2.11

* Ignoring an exception from shutdown (gracefulClose).

## 3.2.10

* Passing client certificate, if any, to Warp
  [#783](https://github.com/yesodweb/wai/pull/783)

## 3.2.9

* Cooperating setGracefulCloseTimeout1 and setGracefulCloseTimeout2 of Warp.
  [#782](https://github.com/yesodweb/wai/pull/782)

## 3.2.8

* Using gracefullClose of network 3.1.1 or later if available.

## 3.2.7

* Relaxing version constraint.

## 3.2.6

* Using the Strict and StrictData language extensions for GHC >8.
  [#752](https://github.com/yesodweb/wai/pull/752)

## 3.2.5

* When tls 1.5.0 is available, TLS 1.3 is automatically supported.

## 3.2.4.3

* Using warp >= 3.2.17.

## 3.2.4.2

* Ignore socket errors while sending `close_notify` [#640](https://github.com/yesodweb/wai/issues/640)

## 3.2.4

* Using tls-session-manager.

## 3.2.3

* Stop using obsoleted APIs of network.

## 3.2.2

* New settting parameter: tlsServerDHEParams [#556](https://github.com/yesodweb/wai/pull/556)
* Preventing socket leakage [#559](https://github.com/yesodweb/wai/pull/559)

## 3.2.1

* Removing dependency to cprng-aes.

## 3.2.0

* Major version up due to breaking changes.
* runHTTP2TLS and runHTTP2TLSSocket were removed.

## 3.1.4

* Add an option to disable HTTP2 [#450](https://github.com/yesodweb/wai/pull/450)

## 3.1.3

* Removing SHA 512 and SHA 384 from supportedCiphers to rescue Safari and golang. [#429](https://github.com/yesodweb/wai/issues/429)

## 3.1.2

* [Getting Rating A from the SSL Server Test](http://www.yesodweb.com/blog/2015/08/ssl-server-test)

## 3.1.1

* Converting "send: resource vanished (Broken pipe)" to ConnectionClosedByPeer. [#421](https://github.com/yesodweb/wai/issues/421)

## 3.1.0

* Supporting HTTP/2 [#399](https://github.com/yesodweb/wai/pull/399)
* Removing RC4 [#400](https://github.com/yesodweb/wai/issues/400)

## 3.0.4.2

* tls 1.3 support [#390](https://github.com/yesodweb/wai/issues/390)

## 3.0.4.1

* Fix for leaked FDs [#378](https://github.com/yesodweb/wai/issues/378)

## 3.0.4

* Replace `acceptSafe` with `accept`, see [#361](https://github.com/yesodweb/wai/issues/361)

## 3.0.3

* Support chain certs [#349](https://github.com/yesodweb/wai/pull/349)

## 3.0.2

* Allow warp-tls to request client certificates. [#337](https://github.com/yesodweb/wai/pull/337)

## 3.0.1.4

Add additional Diffie-Hellman RSA and DSA ciphers to warp-tls.

## 3.0.1.3

[Unable to allow insecure connections with warp-tls #324](https://github.com/yesodweb/wai/issues/324)

## 3.0.1.2

[Make sure Timer is tickled in sendfile. #323](https://github.com/yesodweb/wai/pull/323)

## 3.0.1

[Support for in-memory certificates and keys](https://github.com/yesodweb/wai/issues/301)
