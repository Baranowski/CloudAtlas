all:
	mkdir -p bin
	cabal install --bindir `pwd`/bin --force-reinstalls
