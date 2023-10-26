# ChangeLog for wai

## 3.2.4

* Add helpers for modifying request headers: `modifyRequest` and `mapRequestHeaders`. [#710](https://github.com/yesodweb/wai/pull/710) [#952](https://github.com/yesodweb/wai/pull/952)
* Small documentation adjustments like adding more `@since` markers. [#952](https://github.com/yesodweb/wai/pull/952)
* Add `setRequestBodyChunks` to mirror `getRequestBodyChunk` and avoid deprecation warnings when using `requestBody` as a setter. [#949](https://github.com/yesodweb/wai/pull/949)
* Overhaul documentation of `Middleware`. [#858](https://github.com/yesodweb/wai/pull/858)

## 3.2.3

* Add documentation recommending streaming request bodies. [#818](https://github.com/yesodweb/wai/pull/818)
* Add two functions, `consumeRequestBodyStrict` and `consumeRequestBodyLazy`,
  that are synonyms for `strictRequestBody` and `lazyRequestBody`. [#818](https://github.com/yesodweb/wai/pull/818)

## 3.2.2.1

* Fix missing reexport of `getRequestBodyChunk` [#753](https://github.com/yesodweb/wai/issues/753)

## 3.2.2

* Deprecate `requestBody` in favor of the more clearly named `getRequestBodyChunk`. [#726](https://github.com/yesodweb/wai/pull/726)

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
* The `Network.Wai.HTTP2` module was removed.
* `tryGetFileSize`, `hContentRange`, `hAcceptRanges`, `contentRangeHeader` and
  `chooseFilePart`, `adjustForFilePart` and `parseByteRanges` were removed
  from the `Network.Wai.Internal` module.
* New fields for `Request`: `requestHeaderReferer` and `requestHeaderUserAgent`.

## 3.0.5.0

* Avoid using the `IsString` Builder instance

## 3.0.4.0

* A new module `Network.Wai.HTTP2` is exported.

## 3.0.3.0

* `mapResponseHeaders`, `ifRequest` and `modifyResponse` are exported.

## 3.0.2.3

* Allow blaze-builder 0.4

## 3.0.2.2

* Clarify some documentation on `rawPathInfo`. [Relevant Github
  discussion](https://github.com/yesodweb/wai/issues/325#issuecomment-69896780).
