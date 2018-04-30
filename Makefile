
.PHONY : clean test build


liname: build
	- unlink liname
	- ln -s $(PWD)/.stack-work/install/x86_64-linux-tinfo6-nopie/lts-11.7/8.2.2/bin/liname .


build:
	stack build


stack.yaml: liname.cabal
	stack init --force
