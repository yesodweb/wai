### wai-devel
Development server for [WAI] compliant haskell web applications.

### Installation
- cd wai-devel
- stack build
- stack install

The wai-devel binary will now be in your ~/.local/bin you can use it freely.


### Usage
See the [wiki].

### Issues
Doesn't recompile on changes in Template Haskell files. Here is the [specific issue].


### Coming next
- Provide a dashboard page with compilation status, GC stats, and other such useful meta-information.
- Port to windows. This depends on ide-backend getting ported to Windows.


[WAI]: http://www.yesodweb.com/book/web-application-interface
[wiki]: https://github.com/urbanslug/wai-devel/wiki
[specific issue]: https://github.com/fpco/ide-backend/issues/313
