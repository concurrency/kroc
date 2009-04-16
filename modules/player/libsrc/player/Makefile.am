@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@ --search ../occplayer
CFLAGS = @CFLAGS@ @DEFS@

EXTRA_DIST = \
	debug.occ \
	gen-arcs.ss \
	misc.c \
	misc.inc \
	pioneer_hud.inc \
	pioneerutils.inc \
	player.inc

if BUILD_PLAYER
player_lib_objs = \
	debug.tce \
	misc.o

player_libs = player.lib
player_includes = \
	misc.inc \
	pioneer_hud.inc \
	pioneerutils.inc \
	player.inc \
	points.inc
endif

libs = $(player_libs)
includes = $(player_includes)

all-local: $(libs) $(includes)

player.lib: $(player_lib_objs)
	$(OCCBUILD) --library player.lib --need occplayer --include player.inc $(player_lib_objs)

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
