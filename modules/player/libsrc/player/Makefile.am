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
	sensors.occ

if BUILD_PLAYER
player_lib_objs = \
	control.tce \
	debug.tce \
	hud.tce \
	sensors.tce

player_libs = player.lib
player_includes = player.inc
endif

libs = $(player_libs)
includes = $(player_includes)

all-local: $(libs) $(includes)

player.lib: $(player_lib_objs)
	$(OCCBUILD) --library player.lib --need occplayer --need useful --include player.inc $(player_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(player_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

points.inc: gen-arcs.py
	$(PYTHON) gen-arcs.py >points.inc

CLEANFILES = points.inc

## begin pideps dependencies
control.tce: $(srcdir)/player.inc
control.tce: debug.tce
hud.tce: $(srcdir)/player.inc
hud.tce: $(srcdir)/points.inc
hud.tce: control.tce
hud.tce: debug.tce
sensors.tce: $(srcdir)/player.inc
sensors.tce: debug.tce
## end pideps dependencies
