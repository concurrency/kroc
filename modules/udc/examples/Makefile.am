## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@ --kroc-opts "-e"

EXTRA_DIST = \
	buffered.occ \
	commstime.occ \
	simple.occ \
	socktest.occ \
	softalt.occ \
	softalt2.occ

if BUILD_UDC_EXAMPLES
examples = \
	buffered \
	commstime \
	simple \
	socktest \
	softalt \
	softalt2
endif

all-local: $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce

.occ:
	$(OCCBUILD) --program $<
