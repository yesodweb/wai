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

for pkg in "${pkgs[@]}"; do
  echo "Installing $pkg..."

  (
    cd "./$pkg"

    if ! $CABAL configure --enable-tests --ghc-options="-Wall -Werror" ; then
      $CABAL install --only-dependencies
      $CABAL configure --enable-tests --ghc-options="-Wall -Werror"
    fi

    $CABAL build
    $CABAL test
    $CABAL check
    $CABAL haddock --executables
    ./Setup.lhs install
  )
done
