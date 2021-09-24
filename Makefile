include Makefile.base

.PHONY: exe
exe: build
	stack exec -- ltlspec-exe
