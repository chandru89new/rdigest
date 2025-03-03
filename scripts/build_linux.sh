#!/bin/sh

# install ghcup
sudo apt-get update
sudo apt-get install -y curl g++ libgmp-dev make # Ensure dependencies
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh -s -- -y
export PATH=$PATH:"$HOME/.ghcup/bin"

# install latest ghc and cabal
ghcup install ghc $GHC_VERSION
ghcup set ghc $GHC_VERSION
ghcup install cabal $CABAL_VERSION
ghcup set cabal $CABAL_VERSION

# update cabal dependencies
cabal update

# build binary
make clean-build

# reduce binary size
strip $(readlink -f ./release/rdigest)

# get version
VERSION=$(./release/rdigest version | awk '{print $2}')
echo "VERSION=$VERSION" >> $GITHUB_ENV

# tar-zip binary
BINARY=$(readlink ./release/rdigest)
cd $(dirname "$BINARY")
tar -czvf "$OLDPWD/rdigest_linux_amd64_$VERSION.tar.gz" $(basename "$BINARY")