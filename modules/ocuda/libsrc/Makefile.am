@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@

NVCC = @CUDA_CC@
NVCC_FLAGS = @CUDA_CFLAGS@
NVCC_LIBS = @CUDA_LIBS@


if BUILD_OCUDA
EXTRA_DIST = \
	ocuda_extcode.cu \
	ocuda_wrap.occ \
	ocuda_ctypes.h \
	ocuda.inc

ocuda_lib_objs = \
	ocuda_extcode.o \
	ocuda_wrap.tce

ocuda_libs = ocuda.lib
ocuda_includes = \
	$(srcdir)/ocuda.inc

endif

libs = $(ocuda_libs)
includes = $(ocuda_includes)

all-local: $(libs) $(includes)

ocuda.lib: $(ocuda_lib_objs)
	$(OCCBUILD) --library ocuda.lib --include ocuda.inc $(NVCC_LIBS) $(ocuda_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(ocuda_lib_objs)

SUFFIXES = .occ .tce .lib .cu

.c.o:
	$(CC) $(CFLAGS) -c $<

.cu.o:
	$(NVCC) $(NVCC_FLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

## additional dependencies
ocuda_wrap.tce: ocuda.inc
ocuda_extcode.o: ocuda_ctypes.h

