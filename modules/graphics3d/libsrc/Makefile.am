## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@ --kroc-opts -d

EXTRA_DIST = \
	g3dchess.inc \
	g3dchess.occ \
	g3dnets.occ \
	g3dutil.occ \
	g3dgenerators.occ \
	graphics3d.inc \
	graphics3d.occ \
	rasterutil.occ

if BUILD_GRAPHICS3D
graphics3d_lib_objs = \
	g3dchess.tce \
	g3dnets.tce \
	g3dutil.tce \
	g3dgenerators.tce \
	graphics3d.tce \
	rasterutil.tce

libs = graphics3d.lib
includes = $(srcdir)/graphics3d.inc $(srcdir)/g3dchess.inc
endif

all-local: $(libs)

graphics3d.lib: $(graphics3d_lib_objs)
	$(OCCBUILD) --library graphics3d.lib --need course --need maths --need sdlraster --include graphics3d.inc --include g3dchess.inc $(graphics3d_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean graphics3d.lib $(graphics3d_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

#{{{  dependencies
g3dnets.tce: graphics3d.tce
g3dnets.tce: rasterutil.tce
#}}}

## begin pideps dependencies
g3dchess.tce: $(srcdir)/g3dchess.inc
g3dchess.tce: $(srcdir)/graphics3d.inc
g3dchess.tce: graphics3d.tce
g3dnets.tce: $(srcdir)/graphics3d.inc
g3dnets.tce: graphics3d.tce
g3dnets.tce: g3dgenerators.tce
g3dnets.tce: rasterutil.tce
g3dutil.tce: $(srcdir)/graphics3d.inc
g3dgenerators.tce: $(srcdir)/graphics3d.inc
graphics3d.tce: $(srcdir)/graphics3d.inc
rasterutil.tce: $(srcdir)/graphics3d.inc
## end pideps dependencies
