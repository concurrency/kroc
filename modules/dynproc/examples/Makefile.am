## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	clockproto.inc \
	crashlib.occ \
	crashproto.inc \
	primes.occ \
	sieve.occ \
	sort_pump.occ \
	suspendable.occ \
	testcrash.occ \
	testdyn.occ \
	testdyn2.occ \
	testdyn3.occ \
	testlib.occ \
	testpump.occ \
	testsuspend.occ

examples = \
	primes \
	testdyn \
	testdyn2 \
	testdyn3 \
	testpump \
	testsuspend \
	testcrash
if BUILD_SHARED
dynprocs = \
	sieve.so \
	testlib.so \
	sort_pump.so \
	suspendable.so \
	crashlib.so
endif

all-local: $(examples) $(dynprocs)

clean-local:
	$(OCCBUILD) --clean $(examples)
	rm -f $(dynprocs)

SUFFIXES = .occ .tce .so

.occ:
	$(OCCBUILD) --program $<

.occ.so:
	$(KROC) -l $<
