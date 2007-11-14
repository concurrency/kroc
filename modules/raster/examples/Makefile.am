@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	drawstuff.occ \
	justraster.occ

examples = \
	drawstuff \
	justraster

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples sdlraster --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

%: %.occ
	$(OCCBUILD) --program $<
