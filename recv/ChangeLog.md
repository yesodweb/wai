# ChangeLog for "recv"

## v0.1.3

* Adding `receiveWith`, a `receive` the caller can give up on through an
  `STM` action: nothing is closed and no thread is interrupted to make it
  return.  Requires network 3.2.10.

## v0.1.2

* Providing `receiveNoWait` and `tryWithBufferPool`.
  [#1107](https://github.com/yesodweb/wai/pull/1107)

## v0.1.1

* Fixing the bug that the last chunk is skipped when the size is
  insufficient [#1031](https://github.com/yesodweb/wai/pull/1031)
