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
