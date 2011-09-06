#!/bin/bash -e

# allow a CABAL env var to override
CABAL=${CABAL:-cabal}

# install testing dependencies
$CABAL install HUnit QuickCheck 'hspec >= 0.6.1 && < 0.7'

# Note: Ignoring wai-handler-devel, wai-handler-webkit
pkgs=( wai
       wai-test
       wai-extra
       warp
       wai-app-static
       wai-handler-fastcgi
       wai-handler-launch
       wai-handler-scgi
       warp-static
     )

# install each sub-respository
for pkg in "${pkgs[@]}"; do
  echo "Installing $pkg..."

  (
    cd "./$pkg"

    if [ "$1" = "--clean" ]; then
      $CABAL clean
    fi

    if ! $CABAL configure --ghc-options="-Wall -Werror"; then
      $CABAL install --only-dependencies
      $CABAL configure --ghc-options="-Wall -Werror"
    fi
    $CABAL build

    $CABAL configure --enable-tests 
    $CABAL build
    $CABAL test

    $CABAL check
    if [ "$1" != "--fast" ]; then
      $CABAL haddock --executables
    fi
    ./Setup.lhs install
  )
done
