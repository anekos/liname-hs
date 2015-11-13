
.PHONY : clean test

build: .cabal-sandbox
	cabal --with-ld=ld.gold configure
	cabal --with-ld=ld.gold build

test:
	cabal configure --enable-tests
	cabal build
	cabal test

# d:
# 	cabal install --enable-test --only-dependencies

clean:
	find . -name "*.hi" -exec rm '{}' \;
	find . -name "*.o" -exec rm '{}' \;

dependencies:
	cabal install --only-dependencies
	cabal configure

.cabal-sandbox:
	cabal sandbox init
	cabal install --only-dependencies --enable-tests
	cabal configure --enable-tests
