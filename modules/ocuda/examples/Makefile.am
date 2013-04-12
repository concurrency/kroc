@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4
NVCC = @CUDA_CC@
NVCC_FLAGS = @CUDA_CFLAGS@

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	listdev.occ

if BUILD_OCUDA_EXAMPLES
examples = \
	listdev
endif

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples ocuda --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .cu

.occ:
	$(OCCBUILD) --program $<

.cu.o:
	$(NVCC) $(NVCC_FLAGS) -c $<

