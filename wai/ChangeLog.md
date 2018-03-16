## 3.2.1.2

* Remove dependency on blaze-builder [#683](https://github.com/yesodweb/wai/pull/683)

## 3.2.1.1

* Relax upper bound on bytestring-builder

## 3.2.1

* add mapResponseStatus [#532](https://github.com/yesodweb/wai/pull/532)

## 3.2.0.1

* Add missing changelog entry

## 3.2.0

* Major version up due to breaking changes. We chose 3.2.0, not 3.1.0
  for consistency with Warp 3.2.0.
* The Network.Wai.HTTP2 module was removed.
* tryGetFileSize, hContentRange, hAcceptRanges, contentRangeHeader and
  chooseFilePart, adjustForFilePart and parseByteRanges were removed
  from the Network.Wai.Internal module.
* New fields for Request:  requestHeaderReferer and requestHeaderUserAgent.

## 3.0.5.0

* Avoid using the IsString Builder instance

## 3.0.4.0

* A new module Network.Wai.HTTP2 is exported.

## 3.0.3.0

* mapResponseHeaders, ifRequest and modifyResponse are exported.

## 3.0.2.3

* Allow blaze-builder 0.4

## 3.0.2.2

* Clarify some documentation on `rawPathInfo`. [Relevant Github
  discussion](https://github.com/yesodweb/wai/issues/325#issuecomment-69896780).
