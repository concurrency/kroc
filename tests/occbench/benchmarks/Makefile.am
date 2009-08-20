@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

# Disable usage checking
OCCBUILD = @OCCBUILD@ --occ21-opts '-NU'

EXTRA_DIST = \
	mandelbrot.occ

if BUILD_BENCHMARKS
benchmarks = \
	mandelbrot
endif

all-local: $(benchmarks)

install-exec-local: $(benchmarks)
	$(OCCBUILD) --install-examples occbench --prefix "$(prefix)" --destdir "$(DESTDIR)" $(benchmarks)

clean-local:
	$(OCCBUILD) --clean $(benchmarks)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
