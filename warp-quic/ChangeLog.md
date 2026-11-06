# Revision history for warp-quic

## 0.0.5

A QUIC server is stopped through `settingsInstallShutdownHandler`, as a TCP
one is, and lets its HTTP/3 connections answer what they have taken first.
Requires quic 0.3.15 and http3 0.1.10 for these; not on Windows yet.

* Event poll for accept loop.
  [#1123](https://github.com/yesodweb/wai/pull/1123)
* Let HTTP/3 connections finish when the server stops.

## 0.0.4

* Adjusted code to fit new range of dependency constraints. [#1043](https://github.com/yesodweb/wai/pull/1043)
    * Accept lower versions of `http3` and `tls`

## 0.0.3

* Using http3 v0.1.

## 0.0.2

* Labeling threads.
* New API: runQUICSockets.

## 0.0.1

* Providing `runQUICSocket`.

## 0.0.0

* First version. Released on an unsuspecting world.
