build:
	cabal build --disable-profiling --enable-split-sections && cabal install --overwrite-policy=always --disable-profiling --enable-split-sections --installdir=./release

debug-build:
	cabal build --ghc-options="-prof -fprof-auto -rtsopts" && cabal install --overwrite-policy=always

clean-build:
	cabal clean && make build

start:
	./release/rdigest start

bstart:
	make build && make start
