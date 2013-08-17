
build:
	cabal --with-ld=ld.gold configure
	cabal --with-ld=ld.gold build

i:
	ghci -isrc src/test.hs

ctags:
	cd src && lushtags **/*.hs | tee tags
