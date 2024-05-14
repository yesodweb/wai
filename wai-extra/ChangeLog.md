# Changelog for wai-extra

## 3.1.15

* Added `validateHeadersMiddleware` for validating response headers set by the application [#990](https://github.com/yesodweb/wai/pull/990).

## 3.1.14

* Request parsing throws an exception rather than `error`ing [#972](https://github.com/yesodweb/wai/pull/972):
    * Add `RequestParseException` type and expose it from the `Network.Wai.Parse` module.
    * Behavior change : `parseRequestBody` and `parseRequestBodyEx` (exported from `Network.Wai.Parse`) throw `RequestParseException` rather than calling `error`.
* `defaultGzipSettings` now exported to not depend on `Data.Default` [#959](https://github.com/yesodweb/wai/pull/959)

## 3.1.13.0

* Added `Combine Headers` `Middleware` [#901](https://github.com/yesodweb/wai/pull/901)

## 3.1.12.1

* Include test/{json.gz,noprecompress} as extra-source-files [#887](https://github.com/yesodweb/wai/pull/887)

## 3.1.12

* Added gzip caching based on `ETag` [#885](https://github.com/yesodweb/wai/pull/885):

## 3.1.11

* Overhaul to `Network.Wai.Middleware.Gzip` [#880](https://github.com/yesodweb/wai/pull/880):
    * Don't fail if quality value parameters are present in the `Accept-Encoding` header
    * Add `Accept-Encoding` to the `Vary` response header, instead of overriding it
    * Add setting parameter to decide the compression threshold (`gzipSizeThreshold`)
    * Always skip compression on a `206 Partial Content` response
    * Only catch `IOException`s and `ZlibException`s when using `GzipCacheFolder`
    * Added documentation on the usage of `gzip` and its decision-making.

## 3.1.10.1

* Added documentation to `Accept Override` `Middleware` [#884](https://github.com/yesodweb/wai/pull/884)

## 3.1.10

* Fixed import linting mistake introduced in `3.1.9` ([#875)](https://github.com/yesodweb/wai/pull/875)) where `Network.Wai.Handler.CGI` wouldn't compile on Windows. [#881](https://github.com/yesodweb/wai/pull/880)
* Added `Select` to choose between `Middleware`s [#878](https://github.com/yesodweb/wai/pull/878)

## 3.1.9

* Cleanup and linting of most of `wai-extra` and refactoring the `gzip` middleware to keep it more DRY and to skip compression earlier if possible [#875](https://github.com/yesodweb/wai/pull/875)
* Added `HealthCheckEndpoint` `Middleware`s for health check [#877](https://github.com/yesodweb/wai/pull/877)

## 3.1.8

* Added an `ApacheWithSettings` output format for `RequestLogger` that allows request filtering similar to `DetailedWithSettings` and logging of the current user via wai-logger's `initLoggerUser` [#866](https://github.com/yesodweb/wai/pull/866)

## 3.1.7

* Added new `mPrelogRequests` option to `DetailedSettings` [#857](https://github.com/yesodweb/wai/pull/857)

## 3.1.6

* Remove unused dependencies [#837](https://github.com/yesodweb/wai/pull/837)

## 3.1.5

* `Network.Wai.Middleware.RealIp`: Add a new middleware to infer the remote IP address from headers [#834](https://github.com/yesodweb/wai/pull/834)

## 3.1.4.1

* `Network.Wai.Middleware.Gzip`: Add `Vary: Accept-Encoding` header to responses [#829](https://github.com/yesodweb/wai/pull/829)

## 3.1.4

* Export `Network.Wai.Middleware.RequestLogger.JSON.requestToJSON` [#827](https://github.com/yesodweb/wai/pull/827)

## 3.1.3

* Add a `DetailedWithSettings` output format for `RequestLogger` that allows to hide requests and modify query parameters [#826](https://github.com/yesodweb/wai/pull/826)

## 3.1.2

* Remove an extraneous dot from the error message for `defaultRequestSizeLimitSettings`

## 3.1.1

* `Network.Wai.Middleware.RequestSizeLimit`: Add a new middleware to reject request bodies above a certain size. [#818](https://github.com/yesodweb/wai/pull/818/files)

## 3.1.0

* `Network.Wai.Test`: Add support for source locations to assertion primitives [#817](https://github.com/yesodweb/wai/pull/817)

## 3.0.32

* Undo previous two release, restore code from 3.0.29.2

## 3.0.31

* Undo WaiTestFailure change in previous release

## 3.0.30

* `Network.Wai.Test`: Add support for source locations to assertion primitives [#812](https://github.com/yesodweb/wai/pull/812)

## 3.0.29.2

* flush SSE headers early [#804](https://github.com/yesodweb/wai/pull/804)

## 3.0.29.1

* Fix `Network.Wai.Test.request` always sending an empty request body [#794](https://github.com/yesodweb/wai/pull/794)

## 3.0.29

* Export `Network.Wai.EventSource.eventStreamAppRaw` [#786](https://github.com/yesodweb/wai/pull/786)

## 3.0.28

* Add `Network.Wai.EventSource.eventStreamAppRaw` [#767](https://github.com/yesodweb/wai/pull/767)

## 3.0.27

* Add custom request log formatter which includes response headers [#762](https://github.com/yesodweb/wai/pull/762)

## 3.0.26.1

* When available, supply the response size to custom loggers
  [#757](https://github.com/yesodweb/wai/pull/757)

## 3.0.26

* Throw 413 for too large payload
* Throw 431 for too large headers
  [#741](https://github.com/yesodweb/wai/pull/741)

## 3.0.25

* Supporting `network` version 3.0.

## 3.0.24.3

* Drop unnecessary `lifted-base` dependency
* Drop unnecessary `stringsearch` dependency [#714](https://github.com/yesodweb/wai/pull/714)

## 3.0.24.2

* Consider quoted multipart form boundary markers [#700](https://github.com/yesodweb/wai/pull/700).
* Don't raise exceptions in `formatAsJSON` [#709](https://github.com/yesodweb/wai/pull/709)

## 3.0.24.1

* Fix a "file not found" exception in wai-extra [#705](https://github.com/yesodweb/wai/pull/706)

## 3.0.24.0

* Add timeout middleware [#702](https://github.com/yesodweb/wai/pull/702).

## 3.0.23.0

* Add rewriteRoot middleware [#697](https://github.com/yesodweb/wai/pull/697).

## 3.0.22.1

* Drop dependency on blaze-builder, requiring streaming-commons >= 0.2

## 3.0.22.0

* Support for streaming-commons 0.2
* Support for resourcet 1.2

## 3.0.21.0

* Export `Network.Wai.Parse.noLimitParseRequestBodyOptions` [#662](https://github.com/yesodweb/wai/pull/662).

## 3.0.20.2

* Revert previous change to `srequest`, which caused breakage

## 3.0.20.1

* Set `requestBodyLength` for `srequest` [#654](https://github.com/yesodweb/wai/pull/654)

## 3.0.20.0

* runSessionWith (runSession variant that gives access to ClientState) [#629](https://github.com/yesodweb/wai/pull/629)

## 3.0.19.1

* All loggers follow the autoFlush setting [#604](https://github.com/yesodweb/wai/pull/604)

## 3.0.19

* Add a new function basicAuth', which passes request to the CheckCreds argument.

## 3.0.18

* ForceSSL: preserve port number when redirecting to https. [#582](https://github.com/yesodweb/wai/pull/582)

## 3.0.17

* Gzip pre compressed [#580](https://github.com/yesodweb/wai/pull/580)

## 3.0.16.1

* Fix the way the header length is checked (for limiting the max header length)

## 3.0.16.0

* Add a new function "parseRequestBodyEx" that allows various size limits to be set.

## 3.0.15.3

* Allow wai-logger 2.3

## 3.0.15.2

* Doc improvements

## 3.0.15.1

* don't use deprecated CRT functions on Windows [#544](https://github.com/yesodweb/wai/pull/544)

## 3.0.15

* add requestSizeCheck [#525](https://github.com/yesodweb/wai/pull/525)

## 3.0.14.3

* Add missing `requestHeaderReferer` and `requestHeaderUserAgent` fields to CGI [yesod#1186](https://github.com/yesodweb/yesod/issues/1186)

## 3.0.14.2

* Case insensitive multipart request header lookup [#518](https://github.com/yesodweb/wai/pull/518)

## 3.0.14.1

* Doc update for logStdout and logStdoutDev [#515](https://github.com/yesodweb/wai/issues/515)

## 3.0.14

* Middleware to force domain names. [#506](https://github.com/yesodweb/wai/issues/506) [#507](https://github.com/yesodweb/wai/pull/507)

## 3.0.13.1

* Support wai 3.2

## 3.0.13

* Autoflush handle [#466](https://github.com/yesodweb/wai/pull/466)

## 3.0.12

* Add Network.Wai.Header.contentLength to read the Content-Length header of a response
* The gzip middleware no longer zips responses smaller than 860 bytes

## 3.0.11

* Add constructor for more detailed custom output formats for RequestLogger
* Add JSON output formatter for RequestLogger

## 3.0.10

* Adding Request Body to RequestLogger [#401](https://github.com/yesodweb/wai/pull/401)

## 3.0.9

* Network.Wai.Middleware.Routed module added

## 3.0.7

* Add appearsSecure: check if a request appears to be using SSL even in the
  presence of reverse proxies [#362](https://github.com/yesodweb/wai/pull/362)
* Add ForceSSL middleware [#363](https://github.com/yesodweb/wai/pull/363)
* Add Approot middleware

## 3.0.6.1

* Test code: only include a Cookie header if there are cookies. Without this
  patch, yesod-test cookie handling is broken.

## 3.0.6

* Add Cookie Handling to Network.Wai.Test [#356](https://github.com/yesodweb/wai/pull/356)

## 3.0.5

* add functions to extract authentication data from Authorization header [#352](add functions to extract authentication data from Authorization header #352)

## 3.0.4.6

* Access log sequence not valid [#336](https://github.com/yesodweb/wai/issues/336)

## 3.0.4.5

* Allow fast-logger 2.3

## 3.0.4.3

Test suite warning cleanup

## 3.0.4.2

Allow blaze-builder 0.4

## 3.0.4.1

Fix compilation failure on Windows [#321](https://github.com/yesodweb/wai/issues/321)

## 3.0.4

Add the `StreamFile` middleware.

## 3.0.3

Add the `AddHeaders` middleware.
