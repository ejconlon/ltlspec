#!/bin/bash

# Use: ./script/mkpreso.sh

set -euv

SOURCE_FORMAT="markdown_strict\
+pipe_tables\
+backtick_code_blocks\
+auto_identifiers\
+strikeout\
+yaml_metadata_block\
+implicit_figures\
+all_symbols_escapable\
+link_attributes\
+smart\
+fenced_divs"

mkdir -p output

pushd docs
  time pandoc -s --slide-level 2 --toc \
    --pdf-engine=/Library/TeX/texbin/pdflatex \
    --dpi=300 -V classoption:aspectratio=169 -V lang=en-US --columns=50 -f "${SOURCE_FORMAT}" \
    -M aspectratio=169 --highlight-style=tango \
    -t beamer Presentation.md -o ../output/ltlspec-presentation.pdf
popd
