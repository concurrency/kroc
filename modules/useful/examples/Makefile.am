@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	test-format.occ \
	test-lexer.occ

examples = \
	test-format \
	test-lexer

TESTS_ENVIRONMENT = $(OCCBUILD) --run
TESTS = $(examples)

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples useful --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<

