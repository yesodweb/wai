## time-manager

This package provides module to let you run actions with resettable timeouts
(i.e. `System.TimeManager`) and run actions that will make sure that all
threads forked with the given `ThreadManager` will be killed when the action
finishes.

## WARNINGS

Since version `0.3.0`, **the timeout manager relies on GHC internals**.
This change also means that using **this package only works with the threaded**
**runtime**. The moment a timeout is registered on a non-threaded runtime, an
exception will be thrown.
