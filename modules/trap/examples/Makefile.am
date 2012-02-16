## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	groupops.occ \
	traplatency.occ \
	traptest.occ

if BUILD_TRAP_EXAMPLES
examples = \
	groupops \
	traplatency \
	traptest
endif

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples trap --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce

.occ:
	$(OCCBUILD) --program $<
