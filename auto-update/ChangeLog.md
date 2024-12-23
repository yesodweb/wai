# ChangeLog for auto-update

## 0.2.6

* Using the thread version of AutoUpdate for non-threaded RTS.
  [#1020](https://github.com/yesodweb/wai/pull/1020)

## 0.2.5

* Thread less autoupdate
  [#1018](https://github.com/yesodweb/wai/pull/1018)

## 0.2.4

* Simple refactoring.

## 0.2.3

* [#996](https://github.com/yesodweb/wai/pull/996):
  Refactored the `Control.Debounce` logic to not leak threads.
* [#996](https://github.com/yesodweb/wai/pull/996):
  Added extra `DebounceEdge` options for different types of debouncing.
  * `LeadingMute`: Action on first trigger, and ignore any triggers during cooldown
  * `TrailingDelay`: First trigger starts cooldown, and
    triggers during cooldown extend the cooldown. Action when cooldown expires.

## 0.2.2

* NewAPI: updateThreadName, reaperThreadName, debounceThreadName:
  Names can be given via this field to threads
  for GHC.Conc.Sync.listThreads.

## 0.2.1

* Labeling threads.

## 0.2.0

* Creating Reaper.Internal to export Reaper constructor.
* Hiding Reaper constructor.
* [#985](https://github.com/yesodweb/wai/pull/985):
  Add `reaperModify` to the `Reaper` API, allowing workload modification outside
  of the main `reaperAction` loop.

## 0.1.6

* [#756](https://github.com/yesodweb/wai/pull/756):
  Add control of activation on leading vs. trailing edges for Control.Debounce

## 0.1.5

* [#752](https://github.com/yesodweb/wai/pull/752):
  Using the Strict and StrictData language extensions for GHC >8.

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
