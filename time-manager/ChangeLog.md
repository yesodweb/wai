# ChangeLog for time-manager

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
