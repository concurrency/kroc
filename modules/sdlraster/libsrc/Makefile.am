## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	miniraster.inc \
	miniraster.occ \
	sdlrcompat-c.c \
	sdlrcompat.inc \
	sdlrcompat.occ

sdlraster_lib_objs = \
	sdlrcompat-c.o \
	sdlrcompat.tce

miniraster_lib_objs = \
	miniraster.tce

# FIXME: This package should be buildable without sdlraster.
sdlraster_libs = sdlraster.lib
sdlraster_includes = sdlrcompat.inc
miniraster_libs = miniraster.lib
miniraster_includes = miniraster.inc
libs = $(sdlraster_libs) $(miniraster_libs)
includes = $(sdlraster_includes) $(miniraster_includes)

all-local: $(libs) $(includes)

sdlraster.tce: miniraster.lib

sdlraster.lib: $(sdlraster_lib_objs)  
	$(OCCBUILD) --library sdlraster.lib --need occSDL --need miniraster --include sdlrcompat.inc $(sdlraster_lib_objs)

miniraster.lib: $(miniraster_lib_objs)
	$(OCCBUILD) --library miniraster.lib --need occSDL --need raster --include miniraster.inc $(miniraster_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(sdlraster_lib_objs) $(miniraster_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

