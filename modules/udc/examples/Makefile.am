## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@ --kroc-opts "-e"

EXTRA_DIST = \
	buffered.occ \
	commstime.occ \
	simple.occ \
	socktest.occ \
	softalt.occ \
	softalt2.occ

examples = \
	buffered \
	commstime \
	simple \
	socktest \
	softalt \
	softalt2

all-local: $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce

%: %.occ
	$(OCCBUILD) --program $<
