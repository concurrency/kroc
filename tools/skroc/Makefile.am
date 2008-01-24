
AUTOMAKE_OPTIONS=foreign no-dependencies

MZSCHEME=@MZSCHEME@
MZC=@MZC@

bin_PROGRAMS = skroc

skroc_SOURCES = skroc.ss

SUFFIXES = .ss

LINK=$(MZC) --exe $@ $(basename $@).ss

# deps
skroc skroc: skroc.ss defaults.scm
