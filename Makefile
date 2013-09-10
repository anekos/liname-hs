
build:
	cabal --with-ld=ld.gold configure
	cabal --with-ld=ld.gold build

i:
	cabal-dev ghci

t:
	cabal configure --enable-tests
	cabal build
	cabal test

d:
	cabal install --enable-test --only-dependencies

l: clean build
	hlint src

ctags:
	cd src && lushtags **/*.hs | tee tags

clean:
	find . -name "*.hi" -exec rm '{}' \;
	find . -name "*.o" -exec rm '{}' \;
