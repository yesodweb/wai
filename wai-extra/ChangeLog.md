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
