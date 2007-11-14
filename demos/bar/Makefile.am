@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	bb.occ

examples = \
	bb

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples bar --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

%: %.occ
	$(OCCBUILD) --program $<
