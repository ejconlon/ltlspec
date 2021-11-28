include Makefile.base

.PHONY: exec
exec: build
	stack exec ltlspec-exe

.PHONY: gen
gen: build
	stack exec ltlspec-gen

.PHONY: debug-test
debug-test:
	DEBUG=1 $(MAKE) test

.PHONY: ci-test
ci-test:
	CI=1 $(MAKE) test

.PHONY: docker-test
docker-test:
	docker run -i -v ${PWD}:/project -w /project -t haskell:8.10.7 /bin/bash -c 'make ci-test'

.PHONY: clean-docs
clean-docs:
	rm -f docs/*.{aux,fdb_latexmk,fls,log,nav,out,pdf,snm,synctex.gz,toc}

.PHONY: preso
preso:
	cd docs && pdflatex ./ltlspec-presentation.tex
