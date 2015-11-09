### 22 Aug 2015

- Bumped up the resolver for stack-7.10.yaml from `resolver: ghc-7.10` to `resolver: lts-3.0`
- Changed travis.yaml to reflect guidelines in [https://github.com/commercialhaskell/stack/wiki/FAQ](https://github.com/commercialhaskell/stack/wiki/FAQ)
- Added travis_long from [https://github.com/fpco/ide-backend/blob/master/travis_long](https://github.com/fpco/ide-backend/blob/master/travis_long)
- Removed example cats/
- Cleans up after ghc's -ddump-hi -ddump-to-file
- Fixed [issue #3](https://github.com/urbanslug/wai-devel/issues/3)

### 11 Oct 2015

- Added support for cabal binary.
- Can find dependencies from stack's extra-deps field of stack.yaml
- Added better test coverage.
