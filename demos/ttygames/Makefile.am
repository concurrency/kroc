@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
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
	$(OCCBUILD) --program $<

occman: occman.occ
	$(OCCBUILD) --program $<

mandelbrot: mandelbrot.occ
	$(OCCBUILD) --program $<

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples robots --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
