DIST=dist

default: build

clean:
	rm -rf $(DIST)

build: clean
	cabal configure
	cabal build

install: build
	cabal install

