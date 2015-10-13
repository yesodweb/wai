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
