@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

# Disable usage checking for hereticc, since it's pathologically bad with occ21
# at the moment.
OCCBUILD = @OCCBUILD@ --occ21-opts '-NU'

EXTRA_DIST = \
	hereticc.occ

if BUILD_HERETICC
tests = \
	hereticc
endif

TESTS_ENVIRONMENT = $(OCCBUILD) --run
TESTS = $(tests)

all-local: $(tests)

hereticc: hereticc.occ
	$(OCCBUILD) --program $(srcdir)/hereticc.occ

install-exec-local: $(tests)
	$(OCCBUILD) --install-examples hereticc --prefix "$(prefix)" --destdir "$(DESTDIR)" $(tests)

clean-local:
	$(OCCBUILD) --clean $(tests)

SUFFIXES = .occ
