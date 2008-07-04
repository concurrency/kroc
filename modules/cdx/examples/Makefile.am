@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	cdxblast.c \
	cdxmandel.occ

examples = \
	cdxmandel

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples cdx --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
