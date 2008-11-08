@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	fastvid.occ \
	vidgrab.occ

examples = \
	fastvid \
	vidgrab

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples video --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
