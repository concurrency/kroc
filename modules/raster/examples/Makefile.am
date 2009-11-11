@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	drawstuff.occ \
	imagetool.occ \
	justraster.occ

if BUILD_RASTER_EXAMPLES
examples = \
	drawstuff \
	imagetool \
	justraster
endif

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples sdlraster --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
