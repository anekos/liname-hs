
build:
	cabal --with-ld=ld.gold configure
	cabal --with-ld=ld.gold build

i:
	ghci -isrc src/test.hs

t:
	cabal configure --enable-tests
	cabal build
	cabal test

ctags:
	cd src && lushtags **/*.hs | tee tags

clean:
	find . -name "*.hi" -exec rm '{}' \;
	find . -name "*.o" -exec rm '{}' \;
