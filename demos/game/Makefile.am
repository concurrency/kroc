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

demos = \
	game

all-local: $(demos)

game: game.occ $(game_objs)
	$(OCCBUILD) --program $< $(game_objs)

install-exec-local: $(demos)
	$(OCCBUILD) --install-examples game --prefix "$(prefix)" --destdir "$(DESTDIR)" $(demos)

clean-local:
	$(OCCBUILD) --clean $(demos) $(game_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

#{{{  dependencies
game-bots.occ: game-utils.tce
game-bots.occ: game-objects.tce
game-bots.occ: student-bots.tce
game-client.occ: game-utils.tce
game-client.occ: game-room.tce
game-client.occ: game-objects.tce
game-client.occ: game-bots.tce
game-control.occ: game-utils.tce
game-control.occ: game-client.tce
game-control.occ: game-tty.tce
game-irc.occ: ircplug.tce
game-irc.occ: game-utils.tce
game-irc.occ: game-tty.tce
game-irc.occ: game-client.tce
game-linc.occ: game-utils.tce
game-objects.occ: game-utils.tce
game.occ: game-utils.tce
game.occ: game-room.tce
game.occ: game-objects.tce
game.occ: game-client.tce
game.occ: game-tty.tce
game.occ: game-bots.tce
game.occ: game-linc.tce
game.occ: game-control.tce
game.occ: game-irc.tce
game.occ: game-ttysocket.tce
game-room.occ: game-utils.tce
game-tty.occ: game-utils.tce
game-tty.occ: game-client.tce
game-ttysocket.occ: game-utils.tce
game-ttysocket.occ: game-client.tce
game-ttysocket.occ: game-tty.tce
student-bots.occ: game-utils.tce
student-bots.occ: game-objects.tce
#}}}
