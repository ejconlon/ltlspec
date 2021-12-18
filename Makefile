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
	rm -f docs/*.{aux,bbl,blg,fdb_latexmk,fls,log,nav,out,pdf,snm,synctex.gz,toc}
	rm -rf docs/_minted-ltlspec-report

.PHONY: preso
preso:
	cd docs && pdflatex ./ltlspec-presentation.tex

.PHONY: report-deps
report-deps:
	brew install pygments

.PHONY: report
report:
	cd docs && latexmk -pdf -pdflatex="pdflatex -shell-escape -interaction=nonstopmode" -use-make ./ltlspec-report.tex

.PHONY: full-report
full-report:
	# For some reason having these as make deps is racy. Just run them in sequence.
	$(MAKE) clean-docs
	$(MAKE) gen
	$(MAKE) report
