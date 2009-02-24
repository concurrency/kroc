
AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

MZSCHEME=@MZSCHEME@
MZC=@MZC@

bin_PROGRAMS = skroc

skroc_SOURCES = skroc.ss

SUFFIXES = .ss

LINK=$(MZC) --exe $@ $(basename $@).ss

# deps
skroc skroc: skroc.ss defaults.scm
