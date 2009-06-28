AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

if HAVE_SCHEME_TOOLS
bin_PROGRAMS = skroc
endif

skroc_SOURCES = skroc.ss

SUFFIXES = .ss

LINK = $(MZC) --exe $@ `basename $@`.ss

# deps
skroc skroc: skroc.ss defaults.scm
