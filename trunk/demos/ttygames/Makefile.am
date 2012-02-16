@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	bomberman.occ \
	occman.occ \
	mandelbrot.occ

if BUILD_TTYGAMES
examples = \
	bomberman \
	occman \
	mandelbrot
endif

all-local: $(examples)

bomberman: bomberman.occ
	$(OCCBUILD) --program $(srcdir)/bomberman.occ

occman: occman.occ
	$(OCCBUILD) --program $(srcdir)/occman.occ

mandelbrot: mandelbrot.occ
	$(OCCBUILD) --program $(srcdir)/mandelbrot.occ

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples ttygames --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
