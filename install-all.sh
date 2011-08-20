#!/bin/sh

CABAL=cabal

# install testing dependencies
$CABAL install HUnit QuickCheck hspec
# Note: Ignoring wai-handler-devel, wai-handler-webkit
PACKAGES="wai wai-test wai-extra warp wai-app-static wai-handler-fastcgi wai-handler-launch wai-handler-scgi warp-static"
for package in $PACKAGES
do
    echo Installing $package
    cd $package
    ($CABAL configure --enable-tests ||
        ($CABAL install --only-dependencies && $CABAL configure --enable-tests)
    ) && $CABAL build && $CABAL test && ./Setup.lhs install || exit 1
    cd ..
done
