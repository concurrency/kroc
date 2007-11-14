## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	README \
	barrier_test.occ \
	bucket_test.occ \
	crew_test.occ \
	semaphore_test.occ

examples = \
	barrier_test \
	bucket_test \
	crew_test \
	semaphore_test

all-local: $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce

%: %.occ
	$(OCCBUILD) --program $<
