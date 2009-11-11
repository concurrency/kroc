AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

if HAVE_SCHEME_TOOLS
bin_PROGRAMS = tinyswig
endif

tinyswig_SOURCES = tinyswig.scm

SUFFIXES = .scm

LINK = $(MZC) --exe $@ `basename $@`.scm

# deps
tinyswig tinyswig: tinyswig.scm tinybase.scm

EXTRA_DIST = LICENSE doc.txt inc.c tinyswig-tests.scm
