@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	occbench

SUBDIRS = benchmarks 

TESTS_ENVIRONMENT = \
	$(PYTHON) $(top_srcdir)/occbench \
		--occbuild "$(OCCBUILD)" \
		--bin-dir "$(top_builddir)/benchmarks" \
		--tag "$(TAG)"
TESTS = benchmarks

