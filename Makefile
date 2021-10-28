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
