## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@ --kroc-opts -d

EXTRA_DIST = \
	g3dnets.occ \
	graphics3d.occ \
	graphics3d.inc \
	rasterutil.occ

if BUILD_GRAPHICS3D
graphics3d_lib_objs = \
	g3dnets.tce \
	graphics3d.tce \
	rasterutil.tce

libs = graphics3d.lib
includes = graphics3d.inc
endif

all-local: $(libs)

graphics3d.lib: $(graphics3d_lib_objs)
	$(OCCBUILD) --library graphics3d.lib --need course --need maths --need sdlraster --include graphics3d.inc $(graphics3d_lib_objs)

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
