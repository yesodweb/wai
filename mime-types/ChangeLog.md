## 0.1.2.0

* Added `defaultExtensionMap` to provide the inverse of `defaultMimeMap`.

See PR [#930](https://github.com/yesodweb/wai/pull/930) and [#948](https://github.com/yesodweb/wai/pull/948).

## 0.1.1.0

* Replace `audio/x-mpegurl` with IANA registered type
  `application/vnd.apple.mpegurl`.
* Add TeX-related types; this includes `bib`, `tex`, `sty`, and `cls`.
* Use type image/x-xcf for `.xcf` files.
* Use type "audio/opus" for `.opus` files.
* Add type text/vtt for `.vtt` text track files.
* Use IANA registered type "application/vnd.rar" for `.rar` files.
* Use font types defined in RFC 8081.
* Replace `audio/x-m4a` with `audio/mp4`.
* Change mime type for `.exe` files; use IANA registered type
  `application/vnd.microsoft.portable-executable`.
* Add `video/dv` for files with extension `dv`.
* Use 'application/xml' instead of 'text/xml'.
* Change type for `.pcx` files to `image/vnd.zbrush.pcx`.
* Use `text/markdown` type for `.md` and `.markdown` files.
* Replace `application/x-gzip` with type `application/gzip`.

See PR [#906](https://github.com/yesodweb/wai/pull/906).

## 0.1.0.9

* Add mjs mime type

## 0.1.0.8

* Add wasm mime type

## 0.1.0.7

* Add support for .less files [#534](https://github.com/yesodweb/wai/pull/534)

## 0.1.0.6

* Add woff2 mime type [#350](https://github.com/yesodweb/wai/pull/350)
