
.PHONY : clean test build


liname: build
	- unlink liname
	- ln -s $(shell find .stack-work/install -type f -executable -name liname | head -n 1) .


build:
	stack build


stack.yaml: liname.cabal
	stack init --force


clean:
	stack clean --full
