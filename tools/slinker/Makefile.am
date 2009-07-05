AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

COMMON_SRC = \
	cmdline.ss \
	code-expansion.ss \
	code-improvement.ss \
	code-inspection.ss \
	code-merging.ss \
	code-output.ss \
	code-simplification.ss \
	code-translation-intermediate.ss
	constants.ss \
	helpers.ss \
	types.ss \
	version.ss

if HAVE_SCHEME_TOOLS
bin_PROGRAMS = slinker library2
endif

CLEANFILES = slinker.ss library2.ss

slinker_SOURCES = slinker.ss
EXTRA_slinker_SOURCES = $(COMMON_SRC)
library2_SOURCES = library2.ss
EXTRA_library2_SOURCES = $(COMMON_SRC)

SUFFIXES = .ss .sxml

LINK = $(MZC) --exe $@ `basename $@`.ss

.sxml.ss:
	$(MZSCHEME) -t-- make-driver.ss -f $*

# deps
slinker slinker: slinker.ss $(COMMON_SRC)
library2 library2: library2.ss $(COMMON_SRC)
slinker.exe: slinker.ss $(COMMON_SRC)
library2.exe: library2.ss $(COMMON_SRC)
