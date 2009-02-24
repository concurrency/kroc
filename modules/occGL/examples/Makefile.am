@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	simple_gl.occ

if BUILD_OCCGL_EXAMPLES
examples = \
	simple_gl
endif

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples occGL --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
