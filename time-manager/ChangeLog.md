# ChangeLog for time-manager

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
