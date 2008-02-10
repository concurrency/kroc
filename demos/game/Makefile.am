## process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS=foreign no-dependencies
OCCBUILD=@OCCBUILD@

examples = \
	game

EXTRA_DIST = data \
	game.occ game-utils.occ game-room.occ game-objects.occ students-bots.occ game-bots.occ game-client.occ \
		game-tty.occ game-control.occ game-irc.occ game-ttysocket.occ ircplug.occ game-linc.occ

OCC_OBJECTS = game-utils.o game-room.o game-objects.o student-bots.o game-bots.o game-client.o \
		game-tty.o game-control.o game-irc.o game-ttysocket.o ircplug.o game-linc.o

OCC_TCES = game-utils.tce game-room.tce game-objects.tce student-bots.tce game-bots.tce game-client.tce \
		game-tty.tce game-control.tce game-irc.tce game-ttysocket.tce ircplug.tce game-linc.tce


all-local:	$(examples)

game:		game.occ $(OCC_OBJECTS) config.inc types.inc ircproto.inc
		$(OCCBUILD) --program $< $(OCC_OBJECTS)

clean-local:
		$(OCCBUILD) --clean $(examples) $(OCC_TCES)

game-bots.o: game-utils.o
game-bots.o: game-objects.o
game-bots.o: student-bots.o
game-client.o: game-utils.o
game-client.o: game-room.o
game-client.o: game-objects.o
game-client.o: game-bots.o
game-control.o: game-utils.o
game-control.o: game-client.o
game-control.o: game-tty.o
game-irc.o: ircplug.o
game-irc.o: game-utils.o
game-irc.o: game-tty.o
game-irc.o: game-client.o
game-linc.o: game-utils.o
game-objects.o: game-utils.o
game.o: course.lib.o
game.o: game-utils.o
game.o: game-room.o
game.o: game-objects.o
game.o: game-client.o
game.o: game-tty.o
game.o: game-bots.o
game.o: game-linc.o
game.o: game-control.o
game.o: game-irc.o
game.o: game-ttysocket.o
game-room.o: game-utils.o
game-tty.o: game-utils.o
game-tty.o: game-client.o
game-ttysocket.o: game-utils.o
game-ttysocket.o: game-client.o
game-ttysocket.o: game-tty.o
student-bots.o: game-utils.o
student-bots.o: game-objects.o


SUFFIXES = .occ .tce .lib

.occ.o:
		$(OCCBUILD) --object $<


install-exec-local: $(examples)
	$(mkinstalldirs) "$(DESTDIR)$(datadir)/examples/demos"
	$(INSTALL_PROGRAM) "game" "$(DESTDIR)$(datadir)/examples/demos/game"
	$(mkinstalldirs) "$(DESTDIR)$(datadir)/examples/demos/game-data"
	$(INSTALL_DATA) "data/attic1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/attic1.desc"
	$(INSTALL_DATA) "data/attic2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/attic2.desc"
	$(INSTALL_DATA) "data/attic3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/attic3.desc"
	$(INSTALL_DATA) "data/bar1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/bar1.desc"
	$(INSTALL_DATA) "data/bar2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/bar2.desc"
	$(INSTALL_DATA) "data/bar3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/bar3.desc"
	$(INSTALL_DATA) "data/bar4.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/bar4.desc"
	$(INSTALL_DATA) "data/bathroom.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/bathroom.desc"
	$(INSTALL_DATA) "data/battlements1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/battlements1.desc"
	$(INSTALL_DATA) "data/battlements2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/battlements2.desc"
	$(INSTALL_DATA) "data/battlements3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/battlements3.desc"
	$(INSTALL_DATA) "data/battlements4.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/battlements4.desc"
	$(INSTALL_DATA) "data/battlements5.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/battlements5.desc"
	$(INSTALL_DATA) "data/bed1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/bed1.desc"
	$(INSTALL_DATA) "data/bed2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/bed2.desc"
	$(INSTALL_DATA) "data/bed3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/bed3.desc"
	$(INSTALL_DATA) "data/bed4.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/bed4.desc"
	$(INSTALL_DATA) "data/cc03.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cc03.desc"
	$(INSTALL_DATA) "data/cellar1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cellar1.desc"
	$(INSTALL_DATA) "data/cellar2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cellar2.desc"
	$(INSTALL_DATA) "data/cellar3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cellar3.desc"
	$(INSTALL_DATA) "data/cellar4.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cellar4.desc"
	$(INSTALL_DATA) "data/cellar5.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cellar5.desc"
	$(INSTALL_DATA) "data/cellar6.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cellar6.desc"
	$(INSTALL_DATA) "data/cellar7.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cellar7.desc"
	$(INSTALL_DATA) "data/cellar8.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cellar8.desc"
	$(INSTALL_DATA) "data/construct.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/construct.desc"
	$(INSTALL_DATA) "data/cottage1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cottage1.desc"
	$(INSTALL_DATA) "data/cottage2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cottage2.desc"
	$(INSTALL_DATA) "data/cw10.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw10.desc"
	$(INSTALL_DATA) "data/cw11.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw11.desc"
	$(INSTALL_DATA) "data/cw12.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw12.desc"
	$(INSTALL_DATA) "data/cw13.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw13.desc"
	$(INSTALL_DATA) "data/cw14.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw14.desc"
	$(INSTALL_DATA) "data/cw15.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw15.desc"
	$(INSTALL_DATA) "data/cw16.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw16.desc"
	$(INSTALL_DATA) "data/cw17.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw17.desc"
	$(INSTALL_DATA) "data/cw18.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw18.desc"
	$(INSTALL_DATA) "data/cw19.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw19.desc"
	$(INSTALL_DATA) "data/cw1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw1.desc"
	$(INSTALL_DATA) "data/cw20.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw20.desc"
	$(INSTALL_DATA) "data/cw21.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw21.desc"
	$(INSTALL_DATA) "data/cw22.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw22.desc"
	$(INSTALL_DATA) "data/cw23.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw23.desc"
	$(INSTALL_DATA) "data/cw24.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw24.desc"
	$(INSTALL_DATA) "data/cw25.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw25.desc"
	$(INSTALL_DATA) "data/cw26.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw26.desc"
	$(INSTALL_DATA) "data/cw27.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw27.desc"
	$(INSTALL_DATA) "data/cw28.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw28.desc"
	$(INSTALL_DATA) "data/cw29.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw29.desc"
	$(INSTALL_DATA) "data/cw2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw2.desc"
	$(INSTALL_DATA) "data/cw30.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw30.desc"
	$(INSTALL_DATA) "data/cw31.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw31.desc"
	$(INSTALL_DATA) "data/cw32.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw32.desc"
	$(INSTALL_DATA) "data/cw33.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw33.desc"
	$(INSTALL_DATA) "data/cw34.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw34.desc"
	$(INSTALL_DATA) "data/cw35.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw35.desc"
	$(INSTALL_DATA) "data/cw36.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw36.desc"
	$(INSTALL_DATA) "data/cw37.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw37.desc"
	$(INSTALL_DATA) "data/cw38.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw38.desc"
	$(INSTALL_DATA) "data/cw3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw3.desc"
	$(INSTALL_DATA) "data/cw4.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw4.desc"
	$(INSTALL_DATA) "data/cw5.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw5.desc"
	$(INSTALL_DATA) "data/cw6.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw6.desc"
	$(INSTALL_DATA) "data/cw7.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw7.desc"
	$(INSTALL_DATA) "data/cw8.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw8.desc"
	$(INSTALL_DATA) "data/cw9.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cw9.desc"
	$(INSTALL_DATA) "data/dining.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/dining.desc"
	$(INSTALL_DATA) "data/drawing.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/drawing.desc"
	$(INSTALL_DATA) "data/ducting1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/ducting1.desc"
	$(INSTALL_DATA) "data/ducting2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/ducting2.desc"
	$(INSTALL_DATA) "data/entrance.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/entrance.desc"
	$(INSTALL_DATA) "data/garden1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/garden1.desc"
	$(INSTALL_DATA) "data/garden2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/garden2.desc"
	$(INSTALL_DATA) "data/garden3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/garden3.desc"
	$(INSTALL_DATA) "data/garden4.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/garden4.desc"
	$(INSTALL_DATA) "data/garden5.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/garden5.desc"
	$(INSTALL_DATA) "data/garden6.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/garden6.desc"
	$(INSTALL_DATA) "data/garden7.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/garden7.desc"
	$(INSTALL_DATA) "data/garden8.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/garden8.desc"
	$(INSTALL_DATA) "data/garden9.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/garden9.desc"
	$(INSTALL_DATA) "data/gatehouse.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/gatehouse.desc"
	$(INSTALL_DATA) "data/hall1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/hall1.desc"
	$(INSTALL_DATA) "data/hall2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/hall2.desc"
	$(INSTALL_DATA) "data/kitchen.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/kitchen.desc"
	$(INSTALL_DATA) "data/landing1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/landing1.desc"
	$(INSTALL_DATA) "data/landing2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/landing2.desc"
	$(INSTALL_DATA) "data/landing3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/landing3.desc"
	$(INSTALL_DATA) "data/landing4.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/landing4.desc"
	$(INSTALL_DATA) "data/library.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/library.desc"
	$(INSTALL_DATA) "data/matrix.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/matrix.desc"
	$(INSTALL_DATA) "data/maze10.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/maze10.desc"
	$(INSTALL_DATA) "data/maze11.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/maze11.desc"
	$(INSTALL_DATA) "data/maze12.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/maze12.desc"
	$(INSTALL_DATA) "data/maze1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/maze1.desc"
	$(INSTALL_DATA) "data/maze2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/maze2.desc"
	$(INSTALL_DATA) "data/maze3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/maze3.desc"
	$(INSTALL_DATA) "data/maze4.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/maze4.desc"
	$(INSTALL_DATA) "data/maze5.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/maze5.desc"
	$(INSTALL_DATA) "data/maze6.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/maze6.desc"
	$(INSTALL_DATA) "data/maze7.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/maze7.desc"
	$(INSTALL_DATA) "data/maze8.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/maze8.desc"
	$(INSTALL_DATA) "data/maze9.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/maze9.desc"
	$(INSTALL_DATA) "data/other.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/other.desc"
	$(INSTALL_DATA) "data/pantry.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/pantry.desc"
	$(INSTALL_DATA) "data/patio1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/patio1.desc"
	$(INSTALL_DATA) "data/patio2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/patio2.desc"
	$(INSTALL_DATA) "data/patio3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/patio3.desc"
	$(INSTALL_DATA) "data/shop1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/shop1.desc"
	$(INSTALL_DATA) "data/temple10.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/temple10.desc"
	$(INSTALL_DATA) "data/temple11.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/temple11.desc"
	$(INSTALL_DATA) "data/temple12.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/temple12.desc"
	$(INSTALL_DATA) "data/temple1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/temple1.desc"
	$(INSTALL_DATA) "data/temple2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/temple2.desc"
	$(INSTALL_DATA) "data/temple3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/temple3.desc"
	$(INSTALL_DATA) "data/temple4.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/temple4.desc"
	$(INSTALL_DATA) "data/temple5.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/temple5.desc"
	$(INSTALL_DATA) "data/temple6.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/temple6.desc"
	$(INSTALL_DATA) "data/temple7.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/temple7.desc"
	$(INSTALL_DATA) "data/temple8.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/temple8.desc"
	$(INSTALL_DATA) "data/temple9.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/temple9.desc"
	$(INSTALL_DATA) "data/tunnel.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/tunnel.desc"
	$(INSTALL_DATA) "data/village1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/village1.desc"
	$(INSTALL_DATA) "data/village2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/village2.desc"
	$(INSTALL_DATA) "data/village3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/village3.desc"
	$(INSTALL_DATA) "data/village4.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/village4.desc"
	$(INSTALL_DATA) "data/well.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/well.desc"
	$(INSTALL_DATA) "data/woods10.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/woods10.desc"
	$(INSTALL_DATA) "data/woods11.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/woods11.desc"
	$(INSTALL_DATA) "data/woods1.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/woods1.desc"
	$(INSTALL_DATA) "data/woods2.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/woods2.desc"
	$(INSTALL_DATA) "data/woods3.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/woods3.desc"
	$(INSTALL_DATA) "data/woods4.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/woods4.desc"
	$(INSTALL_DATA) "data/woods5.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/woods5.desc"
	$(INSTALL_DATA) "data/woods6.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/woods6.desc"
	$(INSTALL_DATA) "data/woods7.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/woods7.desc"
	$(INSTALL_DATA) "data/woods8.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/woods8.desc"
	$(INSTALL_DATA) "data/woods9.desc" "$(DESTDIR)$(datadir)/examples/demos/game-data/woods9.desc"

	$(INSTALL_DATA) "data/bread.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/bread.odesc"
	$(INSTALL_DATA) "data/candlestick.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/candlestick.odesc"
	$(INSTALL_DATA) "data/cookies.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/cookies.odesc"
	$(INSTALL_DATA) "data/gateway.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/gateway.odesc"
	$(INSTALL_DATA) "data/gold.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/gold.odesc"
	$(INSTALL_DATA) "data/knife.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/knife.odesc"
	$(INSTALL_DATA) "data/linc-terminal.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/linc-terminal.odesc"
	$(INSTALL_DATA) "data/mace.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/mace.odesc"
	$(INSTALL_DATA) "data/machine.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/machine.odesc"
	$(INSTALL_DATA) "data/mincer.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/mincer.odesc"
	$(INSTALL_DATA) "data/passage.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/passage.odesc"
	$(INSTALL_DATA) "data/photocopier.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/photocopier.odesc"
	$(INSTALL_DATA) "data/photocopy.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/photocopy.odesc"
	$(INSTALL_DATA) "data/ribena.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/ribena.odesc"
	$(INSTALL_DATA) "data/sausage.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/sausage.odesc"
	$(INSTALL_DATA) "data/schriebmann-port.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/schriebmann-port.odesc"
	$(INSTALL_DATA) "data/silver.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/silver.odesc"
	$(INSTALL_DATA) "data/stairs.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/stairs.odesc"
	$(INSTALL_DATA) "data/sword.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/sword.odesc"
	$(INSTALL_DATA) "data/talisman.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/talisman.odesc"
	$(INSTALL_DATA) "data/toothbrush.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/toothbrush.odesc"
	$(INSTALL_DATA) "data/tunnel.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/tunnel.odesc"
	$(INSTALL_DATA) "data/vent.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/vent.odesc"
	$(INSTALL_DATA) "data/well.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/well.odesc"
	$(INSTALL_DATA) "data/wine.odesc" "$(DESTDIR)$(datadir)/examples/demos/game-data/wine.odesc"

	$(INSTALL_DATA) "data/SCRIPT" "$(DESTDIR)$(datadir)/examples/demos/game-data/SCRIPT"

