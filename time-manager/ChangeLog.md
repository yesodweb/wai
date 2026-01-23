# ChangeLog for time-manager

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
