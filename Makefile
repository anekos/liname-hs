
.PHONY : clean test

build: .cabal-sandbox
	cabal --with-ld=ld.gold configure
	cabal --with-ld=ld.gold build

test: .cabal-sandbox
	cabal build
	cabal test

lint: clean build
	hlint src

ctags:
	cd src && lushtags **/*.hs | tee tags

clean:
	find . -name "*.hi" -exec rm '{}' \;
	find . -name "*.o" -exec rm '{}' \;

.cabal-sandbox:
	cabal sandbox init
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests

install-dependencies:
	cabal install --enable-test --only-dependencies
	cabal configure --enable-tests

