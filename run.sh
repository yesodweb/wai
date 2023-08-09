#!/usr/bin/env bash

set -e

GHC="$HOME/ghc/ghc-compare-3/_build/stage1/bin/ghc"
#GHC="$HOME/ghcs-nix/ghcs/9.4.5/bin/ghc"

cabal build -w $GHC warp --write-ghc-environment-file=always
$GHC Repro.hs -threaded -debug
$GHC Test.hs -threaded -debug

run() {
    echo "Starting server..."
    ./Repro +RTS -N2 -v-au 2>&1 &
    sleep 1

    echo "Starting client..."
    ./Test
    echo "Client done"

    sleep 15
    echo "Killing server..."
    kill -INT %1

    echo "Done"
    #nix run nixpkgs#haskellPackages.ghc-events -- show Repro.eventlog
}

run | nix shell nixpkgs#moreutils -c ts -i "%.S"
