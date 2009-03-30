@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	cdxblast.c \
	cdxmandel.occ

if BUILD_CDX_EXAMPLES
examples = \
	cdxmandel
endif

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples cdx --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
