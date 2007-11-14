## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@ --search ../base
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	ppmwrite.occ \
	rasterio.occ

rasterio_lib_objs = \
	ppmwrite.tce \
	rasterio.tce

rasterio_libs = rasterio.lib
rasterio_includes =
libs = $(rasterio_libs)
includes = $(rasterio_includes)

all-local: $(libs) $(includes)

rasterio.lib: $(rasterio_lib_objs)  
	$(OCCBUILD) --library rasterio.lib --need raster --need course --need file $(rasterio_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(rasterio_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<
