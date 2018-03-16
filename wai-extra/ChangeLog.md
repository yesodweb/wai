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
