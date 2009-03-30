
AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

MZSCHEME=@MZSCHEME@
MZC=@MZC@

bin_PROGRAMS = tinyswig

tinyswig_SOURCES = tinyswig.scm

SUFFIXES = .scm

LINK = $(MZC) --exe $@ `basename $@`.scm

# deps
tinyswig tinyswig: tinyswig.scm tinybase.scm

EXTRA_DIST = LICENSE doc.txt inc.c tinyswig-tests.scm
