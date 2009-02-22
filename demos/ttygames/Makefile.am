@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	bomberman.occ \
	occman.occ \
	mandelbrot.occ

examples = \
	bomberman \
	occman \
	mandelbrot

all-local: $(examples)

bomberman: bomberman.occ
	$(OCCBUILD) --program bomberman.occ

occman: occman.occ
	$(OCCBUILD) --program occman.occ

mandelbrot: mandelbrot.occ
	$(OCCBUILD) --program mandelbrot.occ

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples ttygames --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
