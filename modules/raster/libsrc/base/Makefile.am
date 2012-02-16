## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	raster.inc

raster_lib_objs =

raster_libs = raster.lib
raster_includes = $(srcdir)/raster.inc
libs = $(raster_libs)
includes = $(raster_includes)

all-local: $(libs) $(includes)

raster.lib: $(raster_lib_objs)
	$(OCCBUILD) --library raster.lib --include raster.inc $(raster_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(raster_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<
