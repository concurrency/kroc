## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	sdlraster.inc \
	sdlraster.occ \
	sdlrcompat-c.c \
	sdlrcompat.inc \
	sdlrcompat.occ

sdlraster_lib_objs = \
	sdlrcompat.tce \
	sdlraster.tce \
	sdlrcompat-c.o 

sdlraster_libs = sdlraster.lib
sdlraster_includes = sdlraster.inc sdlrcompat.inc
miniraster_libs = miniraster.lib
libs = $(sdlraster_libs) $(miniraster_libs)
includes = $(sdlraster_includes)

all-local: $(libs) $(includes)

sdlraster.lib: $(sdlraster_lib_objs)  
	$(OCCBUILD) --library sdlraster.lib --need occSDL --need raster --include sdlraster.inc --include sdlrcompat.inc $(sdlraster_lib_objs)

# Using miniraster.lib just gets you sdlraster now, since all the compatibility stuff is in one library.
miniraster.lib:
	$(OCCBUILD) --library miniraster.lib --need sdlraster

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(sdlraster_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

#{{{  dependencies
sdlrcompat.occ: sdlraster.tce
#}}}
