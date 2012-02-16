#!/bin/sh
mzscheme -u-- ../../../scripts/wiki2latex.ss -d article -t "RoboDeb Library Documentation" -o lib_docs.moo lib_docs.wiki
pdflatex lib_docs.tex
