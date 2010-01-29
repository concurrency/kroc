@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	test-random.occ \
	test-random.py

examples = \
	test-random

TESTS_ENVIRONMENT = $(OCCBUILD) --run
TESTS = $(examples)

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples random --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
