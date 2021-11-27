include Makefile.base

.PHONY: gendocs
gendocs:
	pdflatex -output-directory=docs -halt-on-error docs/ltlspec-proposal.tex

.PHONY: cleandocs
cleandocs:
	rm -f docs/*.{aux,fdb_latexmk,fls,log,pdf}

.PHONY: exec
exec: build
	stack exec ltlspec-exe

.PHONY: debug-test
debug-test:
	DEBUG=1 $(MAKE) test

.PHONY: ci-test
ci-test:
	CI=1 $(MAKE) test

.PHONY: docker-test
docker-test:
	docker run -i -v ${PWD}:/project -w /project -t haskell:8.10.7 /bin/bash -c 'make ci-test'

.PHONY: preso
preso:
	./script/mkpreso.sh && open ./output/ltlspec-presentation.pdf
