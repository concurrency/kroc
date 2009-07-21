@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@ --search ../occplayer
CFLAGS = @CFLAGS@ @DEFS@

EXTRA_DIST = \
	control.occ \
	debug.occ \
	gen-arcs.py \
	hud.occ \
	player.inc \
	sensors.occ \
	vehicles.occ

if BUILD_PLAYER
player_lib_objs = \
	control.tce \
	debug.tce \
	hud.tce \
	sensors.tce \
	vehicles.tce

player_libs = player.lib
player_includes = $(srcdir)/player.inc
endif

libs = $(player_libs)
includes = $(player_includes)

all-local: $(libs) $(includes)

player.lib: $(player_lib_objs)
	$(OCCBUILD) --library player.lib --need occplayer --need useful --need occSDL --need occGL --need course --need maths --include player.inc $(player_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(player_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

points.inc: $(srcdir)/gen-arcs.py
	$(PYTHON) $(srcdir)/gen-arcs.py >points.inc

CLEANFILES = points.inc

## begin pideps dependencies
control.tce: $(srcdir)/player.inc
control.tce: debug.tce
debug.tce: $(srcdir)/player.inc
hud.tce: $(srcdir)/player.inc
hud.tce: points.inc
hud.tce: control.tce
hud.tce: debug.tce
sensors.tce: $(srcdir)/player.inc
sensors.tce: debug.tce
vehicles.tce: $(srcdir)/player.inc
vehicles.tce: control.tce
vehicles.tce: sensors.tce
## end pideps dependencies
