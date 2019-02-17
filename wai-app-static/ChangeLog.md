## 3.1.6.3

* The executable warp obeys `-h` option properly for host
now. Previously this used to invoke the help option. That can be
reached via `--help` as before.

## 3.1.6.2

* Drop dependency on blaze-builder

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
