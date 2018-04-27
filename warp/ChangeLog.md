## 3.2.22

* Fixing large request body in HTTP/2.

## 3.2.21

* Fixing HTTP/2's timeout handler in request's vault.

## 3.2.20

* Fixing large request body in HTTP/2
  [#593](https://github.com/yesodweb/wai/issues/593)

## 3.2.19

* Fixing 0-byte request body in HTTP/2
  [#597](https://github.com/yesodweb/wai/issues/597)
  [#679](https://github.com/yesodweb/wai/issues/679)

## 3.2.18.2

* Replace dependency on `blaze-builder` with `bsb-http-chunked`

## 3.2.18.1

* Fix benchmark compilation [#681](https://github.com/yesodweb/wai/issues/681)

## 3.2.18

* Make `testWithApplicationSettings` actually use the settings passed.
  [#677](https://github.com/yesodweb/wai/pull/677).

## 3.2.17
* Add support for windows thread block hack and closeOnExec to TLS.
  [#674](https://github.com/yesodweb/wai/pull/674).

## 3.2.16

* In `testWithApplication`, don't `throwTo` ignorable exceptions
  [#671](https://github.com/yesodweb/wai/issues/671), and
  reuse `bindRandomPortTCP`

## 3.2.15

* Address space leak from exception handlers
  [#649](https://github.com/yesodweb/wai/issues/649)

## 3.2.14

* Support streaming-commons 0.2
* Warnings cleanup

## 3.2.13

* Tickling HTTP/2 timer. [624](https://github.com/yesodweb/wai/pull/624)
* Guarantee atomicity of WINDOW_UPDATE increments [622](https://github.com/yesodweb/wai/pull/622)
* Relax HTTP2 headers check [621](https://github.com/yesodweb/wai/pull/621)

## 3.2.12

* If an empty string is set by setServerName, the Server header is not included in response headers [#619](https://github.com/yesodweb/wai/issues/619)

## 3.2.11.2

* Don't throw exceptions when closing a keep-alive connection
  [#618](https://github.com/yesodweb/wai/issues/618)

## 3.2.11.1

* Move exception handling to top of thread (fixes
  [#613](https://github.com/yesodweb/wai/issues/613))

## 3.2.11

* Fixing 10 HTTP2 bugs pointed out by h2spec v2.

## 3.2.10

* Add `connFree` to `Connection`. Close socket connections on timeout triggered. Timeout exceptions extend from `SomeAsyncException`. [#602](https://github.com/yesodweb/wai/pull/602) [#605](https://github.com/yesodweb/wai/pull/605)

## 3.2.9

* Fixing a space leak. [#586](https://github.com/yesodweb/wai/pull/586)

## 3.2.8

* Fixing HTTP2 requestBodyLength. [#573](https://github.com/yesodweb/wai/pull/573)
* Making HTTP/2 :path optional for the CONNECT method. [#572](https://github.com/yesodweb/wai/pull/572)
* Adding new APIs for HTTP/2 trailers: http2dataTrailers and modifyHTTP2Data [#566](https://github.com/yesodweb/wai/pull/566)

## 3.2.7

* Adding new APIs for HTTP/2 server push: getHTTP2Data and setHTTP2Data [#510](https://github.com/yesodweb/wai/pull/510)
* Better accept(2) error handling [#553](https://github.com/yesodweb/wai/pull/553)
* Adding getGracefulShutdownTimeout.
* Add {test,}withApplicationSettings [#531](https://github.com/yesodweb/wai/pull/531)

## 3.2.6

* Using token based APIs of http2 1.6.

## 3.2.5

* Ignoring errors from setSocketOption. [#526](https://github.com/yesodweb/wai/issues/526).

## 3.2.4

* Added `withApplication`, `testWithApplication`, and `openFreePort`
* Fixing reaper delay value of file info cache.

## 3.2.3

* Using http2 v1.5.x which much improves the performance of HTTP/2.
* To get rid of the bottleneck of ByteString's (==), a new logic to
  compare header names is introduced.

## 3.2.2

* Throwing errno for pread [#499](https://github.com/yesodweb/wai/issues/499).
* Makeing compilable on Windows [#505](https://github.com/yesodweb/wai/issues/505).

## 3.2.1

* Add back `warpVersion`

## 3.2.0

* Major version up due to breaking changes. This is because the HTTP/2 code
  was started over with Warp 3.1.3 due to performance issue [#470](https://github.com/yesodweb/wai/issues/470).
* runHTTP2, runHTTP2Env, runHTTP2Settings and runHTTP2SettingsSocket were removed from the Network.Wai.Handler.Warp module.
* The performance of HTTP/2 was drastically improved. Now the performance of HTTP/2 is almost the same as that of HTTP/1.1.
* The logic to handle files in HTTP/2 is now identical to that in HTTP/1.1.
* Internal stuff was removed from the Network.Wai.Handler.Warp module according to [the plan](http://www.yesodweb.com/blog/2015/06/cleaning-up-warp-apis).

## 3.1.12

* Setting lower bound for auto-update [#495](https://github.com/yesodweb/wai/issues/495)

## 3.1.11

* Providing a new API: killManager.
* Preventing space leaks due to Weak ThreadId [#488](https://github.com/yesodweb/wai/issues/488)
* Setting upper bound for http2.

## 3.1.10

* `setFileInfoCacheDuration`
* `setLogger`
* `FileInfo`/`getFileInfo`
* Fix: warp-tls strips out the Host request header [#478](https://github.com/yesodweb/wai/issues/478)

## 3.1.9

* Using the new priority queue based on PSQ provided by http2 lib again.

## 3.1.8

* Using the new priority queue based on PSQ provided by http2 lib.

## 3.1.7

* A concatenated Cookie header is prepended to the headers to ensure that it flows pseudo headers. [#454](https://github.com/yesodweb/wai/pull/454)
* Providing a new settings: `setHTTP2Disabled` [#450](https://github.com/yesodweb/wai/pull/450)

## 3.1.6

* Adding back http-types 0.8 support [#449](https://github.com/yesodweb/wai/pull/449)

## 3.1.5

* Using	http-types v0.9.
* Fixing build on OpenBSD. [#428](https://github.com/yesodweb/wai/pull/428) [#440](https://github.com/yesodweb/wai/pull/440)
* Fixing build on Windows. [#438](https://github.com/yesodweb/wai/pull/438)

## 3.1.4

* Using newer http2 library to prevent change table size attacks.
* API for HTTP/2 server push and trailers. [#426](https://github.com/yesodweb/wai/pull/426)
* Preventing response splitting attacks. [#435](https://github.com/yesodweb/wai/pull/435)
* Concatenating multiple Cookie: headers in HTTP/2.

## 3.1.3

* Warp now supports blaze-builder v0.4 or later only.
* HTTP/2 code was improved: dynamic priority change, efficient queuing and sender loop continuation. [#423](https://github.com/yesodweb/wai/pull/423) [#424](https://github.com/yesodweb/wai/pull/424)

## 3.1.2

* Configurable Slowloris size [#418](https://github.com/yesodweb/wai/pull/418)

## 3.1.1

* Fixing a bug of HTTP/2 when no FD cache is used [#411](https://github.com/yesodweb/wai/pull/411)
* Fixing a buffer-pool bug [#406](https://github.com/yesodweb/wai/pull/406) [#407](https://github.com/yesodweb/wai/pull/407)

## 3.1.0

* Supporting HTTP/2 [#399](https://github.com/yesodweb/wai/pull/399)
* Cleaning up APIs [#387](https://github.com/yesodweb/wai/issues/387)

## 3.0.13.1

* Remove dependency on the void package [#375](https://github.com/yesodweb/wai/pull/375)

## 3.0.13

* Turn off file descriptor cache by default [#371](https://github.com/yesodweb/wai/issues/371)

## 3.0.12.1

* Fix for: HEAD requests returning non-empty entity body [#369](https://github.com/yesodweb/wai/issues/369)

## 3.0.12

* Only conditionally produce HTTP 100 Continue

## 3.0.11

* Better HEAD support for files [#357](https://github.com/yesodweb/wai/pull/357)

## 3.0.10

* Fix [missing `IORef` tweak](https://github.com/yesodweb/wai/issues/351)
* Disable timeouts as soon as request body is fully consumed. This addresses
  the common case of a non-chunked request body. Previously, we would wait
  until a zero-length `ByteString` is returned, but that is suboptimal for some
  cases. For more information, see [issue
  351](https://github.com/yesodweb/wai/issues/351).
* Add `pauseTimeout` function

## 3.0.9.3

* Don't serve a 416 status code for 0-length files [keter issue #75](https://github.com/snoyberg/keter/issues/75)
* Don't serve content-length for 416 responses [#346](https://github.com/yesodweb/wai/issues/346)

## 3.0.9.2

Fix support for old versions of bytestring

## 3.0.9.1

Add support for blaze-builder 0.4

## 3.0.9

* Add runEnv: like run but uses $PORT [#334](https://github.com/yesodweb/wai/pull/334)

## 3.0.5.2

* [Pass the Request to settingsOnException handlers when available. #326](https://github.com/yesodweb/wai/pull/326)

## 3.0.5

Support for PROXY protocol, such as used by Amazon ELB TCP. This is useful
since, for example, Amazon ELB HTTP does *not* have support for Websockets.
More information on the protocol [is available from
Amazon](http://docs.aws.amazon.com/ElasticLoadBalancing/latest/DeveloperGuide/TerminologyandKeyConcepts.html#proxy-protocol).

## 3.0.4

Added `setFork`.

## 3.0.3

Modify flushing of request bodies. Previously, regardless of the size of the
request body, the entire body would be flushed. When uploading large files to a
web app that does not accept such files (e.g., returns a 413 too large status),
browsers would still send the entire request body and the servers will still
receive it.

The new behavior is to detect if there is a large amount of data still to be
consumed and, if so, immediately terminate the connection. In the case of
chunked request bodies, up to a maximum number of bytes is consumed before the
connection is terminated.

This is controlled by the new setting `setMaximumBodyFlush`. A value of
@Nothing@ will return the original behavior of flushing the entire body.

## 3.0.0

WAI no longer uses conduit for its streaming interface.

## 2.1.0

The `onOpen` and `onClose` settings now provide the `SockAddr` of the client,
and `onOpen` can return a `Bool` which will close the connection. The
`responseRaw` response has been added, which provides a more elegant way to
handle WebSockets than the previous `settingsIntercept`. The old settings
accessors have been deprecated in favor of new setters, which will allow
settings changes to be made in the future without breaking backwards
compatibility.

## 2.0.0

ResourceT is not used anymore. Request and Response is now abstract data types.
To use their constructors, Internal module should be imported.

## 1.3.9

Support for byte range requests.

## 1.3.7

Sockets now have `FD_CLOEXEC` set on them. This behavior is more secure, and
the change should not affect the vast majority of use cases. However, it
appeared that this is buggy and is fixed in 2.0.0.
