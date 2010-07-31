## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	sdlraster.inc \
	sdlraster.occ \
	sdlrcompat-c.c \
	sdlrcompat.inc \
	sdlrcompat.occ

if BUILD_SDLRASTER
sdlraster_lib_objs = \
	sdlraster.tce \
	sdlrcompat.tce \
	sdlrcompat-c.o

sdlraster_libs = sdlraster.lib miniraster.lib
sdlraster_includes = $(srcdir)/sdlraster.inc $(srcdir)/sdlrcompat.inc
endif

libs = $(sdlraster_libs) $(miniraster_libs)
includes = $(sdlraster_includes)

all-local: $(libs) $(includes)

sdlraster.lib: $(sdlraster_lib_objs)
	$(OCCBUILD) --library sdlraster.lib --need occSDL --need raster --include sdlraster.inc --include sdlrcompat.inc $(sdlraster_lib_objs)

# Using miniraster.lib just gets you sdlraster now, since all the compatibility stuff is in one library.
miniraster.lib: sdlraster.lib
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
sdlrcompat.tce: sdlraster.tce
#}}}
