@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	fastvid.occ \
	vidgrab.occ

if BUILD_VIDEO_EXAMPLES
examples = \
	fastvid \
	vidgrab
endif

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples video --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
