# wai-app-static changelog

## 3.1.9

* Added `NoCache` constructor to `MaxAge` [#977](https://github.com/yesodweb/wai/pull/977)

## 3.1.8

* Added `NoStore` constructor to `MaxAge` [#938](https://github.com/yesodweb/wai/pull/938)

## 3.1.7.5

* Removed dependency of `time`, `old-locale` and `network` [#902](https://github.com/yesodweb/wai/pull/902)

## 3.1.7.4

* Fix a bug when the cryptonite flag is disabled. [#874](https://github.com/yesodweb/wai/pull/874)

## 3.1.7.3

* Introduce a flag to avoid the cryptonite dependency. [#871](https://github.com/yesodweb/wai/pull/871)

## 3.1.7.2

* `optparse-applicative-0.16.0.0` support

## 3.1.7.1

* Update the test suite too

## 3.1.7

* Use 302 instead of 301 redirect, to avoid caching the presence of an index.html file

## 3.1.6.3

* The executable warp obeys `-h` option properly for host
now. Previously this used to invoke the help option. That can be
reached via `--help` as before.

## 3.1.6.2

* Drop dependency on `blaze-builder`

## 3.1.6.1

* Add `<>` import

## 3.1.6

* Make ssAddTrailingSlash work in combination with ssIndices [#569](https://github.com/yesodweb/wai/pull/569)
* Make ssIndices work with ssLookupFile and trailing slashes [#570](https://github.com/yesodweb/wai/pull/570)

## 3.1.5

* Switch to cryponite

## 3.1.4.1

* Support wai/warp 3.2

## 3.1.4

* Reinstate redirectToIndex

## 3.1.3

* Add 404 handler [#467](https://github.com/yesodweb/wai/pull/467)

## 3.1.2

* Honor ssIndices when used with defaultWebAppSettings [#460](https://github.com/yesodweb/wai/pull/460)

## 3.1.1

* Make adding a trailing slash optional [#327](https://github.com/yesodweb/wai/issues/327) [yesod#988](https://github.com/yesodweb/yesod/issues/988)

## 3.1.0

* Drop system-filepath

## 3.0.1.1

* Fix root links

## 3.0.1

* Better HEAD support [#354](https://github.com/yesodweb/wai/issues/354)

## 3.0.0.6

Fix trailing slashes for `UrlMap` and other non-root setups [#325](https://github.com/yesodweb/wai/issues/325)

## 3.0.0.4

Add missing trailing slashes [#312](https://github.com/yesodweb/wai/issues/312)

## 3.0.0.3

Support for time 1.5
