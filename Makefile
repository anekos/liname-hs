
.PHONY : clean test build


liname: build
	- unlink liname
	- ln -s $(PWD)/.stack-work/install/x86_64-linux/lts-6.9/7.10.3/bin/liname .


build:
	stack build


stack.yaml: liname.cabal
	stack init --force
