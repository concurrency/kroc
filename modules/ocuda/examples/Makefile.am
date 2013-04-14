@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4
NVCC = @CUDA_CC@
NVCC_FLAGS = @CUDA_CFLAGS@

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	listdev.occ
	mandelgpu.occ
	mandelgpu_types.inc
	mandelcuda.cu

if BUILD_OCUDA_EXAMPLES
ocuda_examples = \
	listdev
endif
if BUILD_OCUDA_SDL_EXAMPLES
ocuda_sdl_examples = \
	mandelgpu
endif

examples = $(ocuda_examples) $(ocuda_sdl_examples)

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples ocuda --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

mandelgpu: mandelgpu.occ mandelgpu_types.inc mandelcuda.o
	$(OCCBUILD) --program $(srcdir)/mandelgpu.occ mandelcuda.o

SUFFIXES = .occ .cu

.occ:
	$(OCCBUILD) --program $<

.cu.o:
	$(NVCC) $(NVCC_FLAGS) -c $<

