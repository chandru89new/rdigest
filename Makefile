.PHONY: build

build:
	make build-ui && cabal build --disable-profiling --enable-split-sections && cabal install --overwrite-policy=always --disable-profiling --enable-split-sections --installdir=./release

build-cli: 
	cabal build --disable-profiling --enable-split-sections && cabal install --overwrite-policy=always --disable-profiling --enable-split-sections --installdir=./release

build-ui: 
	cd ui && yarn build

build-ui-dev: 
	cd ui && yarn build-dev

debug-build:
	cabal build --ghc-options="-prof -fprof-auto -rtsopts" && cabal install --overwrite-policy=always

clean-build:
	cabal clean && make build

build-dev:
	make build-ui-dev && make build-cli

start:
	./release/rdigest start

