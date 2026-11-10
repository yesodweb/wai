# ChangeLog for time-manager

## 0.5.3

* New `System.Watchdog`: one timeout supervisor per connection. The
  connection records what it is doing in a context, and the watchdog
  decides from that alone whether it has been doing it for too long. A
  timeout is reported through a `TVar`, to be composed with what the
  connection waits for in STM, rather than thrown.

  What the context holds and what counts as too long are the caller's:
  a layer gives a type to `WatchdogFor`, where `ContextFor` says what it
  records and `decide` says what the watchdog should do when that record
  changes. A connection is watched by one layer at a time and is passed
  to the next with `handOver`.
  [#1122](https://github.com/yesodweb/wai/pull/1122)

## 0.5.2

* A timeout now fires on Windows under the old I/O manager, which is
  still the default there. The timer was `GHC.Event.Windows`, WinIO's
  own, so nothing ran the action unless the program used WinIO: warp
  timed no connection out, and neither did anything else on top.
  [#1139](https://github.com/yesodweb/wai/issues/1139)
* `tickle` extends a timeout by microseconds on Windows as it does
  everywhere else.  It floored the value to a whole second there, which
  meant a timeout under a second was never extended at all.

## 0.5.1

* A renewed timeout compared nanoseconds with microseconds, so a timeout
  renewed after its callback had been queued ran at once instead of being
  rescheduled for the time it had just been given. Wrong since 0.4.0.
  [#1134](https://github.com/yesodweb/wai/pull/1134)

## 0.5.0

* BREAKING CHANGE: a `ThreadManager` no longer carries a `T.Manager`.
  `newThreadManager` takes no argument, `forkManagedTimeout` and
  `forkManagedTimeoutFinally` take the caller's, and `withHandle` and the
  re-exports from `System.TimeManager` are gone.
  [#1131](https://github.com/yesodweb/wai/pull/1131)
* Count a thread in `System.ThreadManager` from when it is forked rather
  than from when it starts, with `forkManagedBy`, `reserveManaged`,
  `takeReservation` and `countManaged`.
  [#1128](https://github.com/yesodweb/wai/pull/1128)
* Keep counting the threads `stopAfter` stopped until they end.
  [#1128](https://github.com/yesodweb/wai/pull/1128)
* `countManaged` throws `AssertionFailed` when a reservation has been taken
  by a manager other than its own.
  [#1128](https://github.com/yesodweb/wai/pull/1128)
* `forkManagedFinally` and `forkManagedTimeoutFinally` no longer swallow an
  exception from the action; it ends the thread, after the cleanup has run.
  [#1128](https://github.com/yesodweb/wai/pull/1128)
* Add `forkManagedUnmaskFinally`, so that every fork has a variant with a
  cleanup.
  [#1128](https://github.com/yesodweb/wai/pull/1128)

## 0.4.0

* CHANGES IN BEHAVIOUR:
  * `tickle` is rate-limited/debounced. The renewal is skipped unless a quarter
    of the timeout (capped at one second) has passed since the timeout was last
    registered or updated. This does mean a timeout _might_ run a bit earlier
    than the last `tickle` would indicate, but never more than the maximum
    debounce period.
  * `cancel` completely stops the timeout, making it un`resume`able.
    `resume` will only resume a timeout that has been `pause`d.
  * Prior to this major version, the `Handle` could be reused to run more
    timeout actions. Now, a timeout action will only ever run, at most, once.
    After a timeout action has run, the `Handle` is turned off and won't be
    `resume`able, necessitating a call to `register` to start a new timeout.

  [#1109](https://github.com/yesodweb/wai/pull/1109)

## 0.3.2

* Add `stopAfterWithResult`. [#1069](https://github.com/yesodweb/wai/pull/1069)

## 0.3.1.1

* Added `README.md` file and improved documentation of modules and functions.
  [#1057](https://github.com/yesodweb/wai/pull/1057)

## 0.3.1

BUGFIXES:

* [#1055](https://github.com/yesodweb/wai/pull/1055)
  * `resume` now acts as a `tickle` if the `Handle` isn't paused.
    This is the same behaviour as before version `0.3.0`.
  * `registerKillThread` now throws the `TimeoutThread` in a separate
    thread, so as to not block the GHC's System TimerManager.
    This does mean that `TimeoutThread` exceptions could technically
    be thrown "out of order", but they will be more prompt.


## 0.3.0

* [#1048](https://github.com/yesodweb/wai/pull/1048)
  * New architecture. The backend is switched from the designated thread
    to GHC's System TimerManager. From this version, this library is
    just wrapper APIs of GHC's System TimerManager. Unlike v0.2 or
    earlier, callbacks are executed at the exact time. System
    TimerManager uses a PSQ (a tree) while v0.2 or earlier uses a list.
    So, this version hopefully scales better.
  * Deprecated functions: `stopManager`, `killManager` and `withManager'`.
  * `tickle` sets the specified timeout from now.
  * `pause` is now identical to `cancel`.
  * `resume` is now re-registration of timeout.
  * The signature of `withHandle` is changed.

This change also means that using this package only works with the threaded runtime.
The moment a timeout is registered on a non-threaded runtime, an exception will be thrown.

## 0.2.4

* Providing `isAllGone`.
* Providing emptyHandle.

## 0.2.3

* Exporting defaultManager.

## 0.2.2

* `initialize` with non positive integer creates a time manager
  which does not maintain timeout.
  [#1017](https://github.com/yesodweb/wai/pull/1017)

## 0.2.1

* Export KilledByThreadManager exception
  [#1016](https://github.com/yesodweb/wai/pull/1016)

## 0.2.0

* Providing `System.ThreadManager`.
* `withHandle` catches `TimeoutThread` internally.
  It returns `Nothing` on timeout.

## 0.1.3

* Providing `withHandle` and `withHandleKillThread`.

## 0.1.2

* Holding `Weak ThreadId` to prevent thread leak again
  [#1013](https://github.com/yesodweb/wai/pull/1013)

## 0.1.1

* Removing `unliftio`.

## 0.1.0

* [#986](https://github.com/yesodweb/wai/pull/986)
    * Change behavior of `cancel` to immediately remove the `Handle` from the
    reaper's workload, rather than waiting for timeout.
    * Using auto-update v0.2.0.
