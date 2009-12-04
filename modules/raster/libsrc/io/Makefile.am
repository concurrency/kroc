## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

if HAVE_LIBPNG
png_flags = -DHAVE.LIBPNG
endif

OCCBUILD = @OCCBUILD@ --search ../base $(png_flags)
CFLAGS = @CFLAGS@ @DEFS@ @LIBPNG_CFLAGS@

EXTRA_DIST = \
	png-c.c \
	png.occ \
	ppmread.occ \
	ppmwrite.occ \
	rasterio.occ

if HAVE_LIBPNG
png_objs = \
	png-c.o \
	png.tce

png_ldadd = @LIBPNG_LIBS@
endif

rasterio_lib_objs = \
	$(png_objs) \
	ppmread.tce \
	ppmwrite.tce \
	rasterio.tce

rasterio_libs = rasterio.lib
rasterio_includes =
libs = $(rasterio_libs)
includes = $(rasterio_includes)

all-local: $(libs) $(includes)

rasterio.lib: $(rasterio_lib_objs)  
	$(OCCBUILD) --library rasterio.lib --need raster --need course --need file $(png_ldadd) $(rasterio_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(rasterio_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

rasterio.tce: ppmread.tce ppmwrite.tce $(png_objs)
