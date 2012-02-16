@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	menutest.occ

if BUILD_TTYUTIL_EXAMPLES
examples = \
	menutest
endif

all-local: $(examples)

menutest: menutest.occ
	$(OCCBUILD) --program $(srcdir)/menutest.occ

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples ttyutil --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

