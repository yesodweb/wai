# ChangeLog for auto-update

## 0.2.1

* Labeling threads.

## 0.2.0

* Creating Reaper.Internal to export Reaper constructor.
* Hiding Reaper constructor.
* Add `reaperModify` to the `Reaper` API, allowing workload modification outside
  of the main `reaperAction` loop.
  [#985](https://github.com/yesodweb/wai/pull/985)

## 0.1.6

* Add control of activation on leading vs. trailing edges for Control.Debounce
  [#756](https://github.com/yesodweb/wai/pull/756)

## 0.1.5

* Using the Strict and StrictData language extensions for GHC >8.
  [#752](https://github.com/yesodweb/wai/pull/752)

## 0.1.4.1

* [#693](https://github.com/yesodweb/wai/pull/693):
  Improve documentation for `reaperAction` function.
* [#732](https://github.com/yesodweb/wai/pull/732):
  Fixed memory leak in `reaperAdd` function.

## 0.1.4

* Provide updateActionModify API in AutoUpdate [#547](https://github.com/yesodweb/wai/pull/547)

## 0.1.3.1

* Doc improvements

## 0.1.3

* Adding a new AIP - reaperKill

## 0.1.2

* Added Control.Debounce
