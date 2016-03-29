#!/bin/sh

set -eux

# For simplicity, installing cabal-meta and Stack for both the Cabal and Stack
# build plans, even though they're never both necessary.

mkdir -p ~/.local/bin
if [ `uname` = "Darwin" ]
then
  curl --insecure -L https://www.stackage.org/stack/osx-x86_64 | tar xz --strip-components=1 --include '*/stack' -C ~/.local/bin
  # brew no longer has fcgi
  # brew install fcgi
  grep -v wai-handler-fastcgi < stack.yaml > tmp
  mv tmp stack.yaml
else
  curl -L https://www.stackage.org/stack/linux-x86_64 | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'
  curl -L http://download.fpcomplete.com/michael/cabal-meta.tar.gz | tar xz -C $HOME/.local/bin
fi

mkdir -p $HOME/.cabal
cat > $HOME/.cabal/config <<EOF
remote-repo: hackage.haskell.org:http://hackage.fpcomplete.com/
remote-repo-cache: $HOME/.cabal/packages
jobs: \$ncpus
EOF
