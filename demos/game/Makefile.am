## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

OCCBUILD = @OCCBUILD@ -DBASEPATH="$(datadir)/kroc/game"

SUBDIRS = data

EXTRA_DIST = \
	config.inc \
	game-bots.occ \
	game-client.occ \
	game-control.occ \
	game-irc.occ \
	game-linc.occ \
	game-objects.occ \
	game-room.occ \
	game-tty.occ \
	game-ttysocket.occ \
	game-utils.occ \
	game.occ \
	ircplug.occ \
	ircproto.inc \
	student-bots.occ \
	types.inc

game_objs = \
	game-bots.tce \
	game-client.tce \
	game-control.tce \
	game-irc.tce \
	game-linc.tce \
	game-objects.tce \
	game-room.tce \
	game-tty.tce \
	game-ttysocket.tce \
	game-utils.tce \
	ircplug.tce \
	student-bots.tce

if BUILD_GAME
demos = \
	game
endif

all-local: $(demos)

game: game.occ $(game_objs)
	$(OCCBUILD) --program $(srcdir)/game.occ $(game_objs)

install-exec-local: $(demos)
	$(OCCBUILD) --install-examples game --prefix "$(prefix)" --destdir "$(DESTDIR)" $(demos)

clean-local:
	$(OCCBUILD) --clean $(demos) $(game_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

#{{{  dependencies
game-bots.tce: game-utils.tce
game-bots.tce: game-objects.tce
game-bots.tce: student-bots.tce
game-client.tce: game-utils.tce
game-client.tce: game-room.tce
game-client.tce: game-objects.tce
game-client.tce: game-bots.tce
game-control.tce: game-utils.tce
game-control.tce: game-client.tce
game-control.tce: game-tty.tce
game-irc.tce: ircplug.tce
game-irc.tce: game-utils.tce
game-irc.tce: game-tty.tce
game-irc.tce: game-client.tce
game-linc.tce: game-utils.tce
game-objects.tce: game-utils.tce
game.tce: game-utils.tce
game.tce: game-room.tce
game.tce: game-objects.tce
game.tce: game-client.tce
game.tce: game-tty.tce
game.tce: game-bots.tce
game.tce: game-linc.tce
game.tce: game-control.tce
game.tce: game-irc.tce
game.tce: game-ttysocket.tce
game-room.tce: game-utils.tce
game-tty.tce: game-utils.tce
game-tty.tce: game-client.tce
game-ttysocket.tce: game-utils.tce
game-ttysocket.tce: game-client.tce
game-ttysocket.tce: game-tty.tce
student-bots.tce: game-utils.tce
student-bots.tce: game-objects.tce
#}}}
