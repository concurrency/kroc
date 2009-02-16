@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	graphics.occ robot2.occ robot3.occ robot.occ \
	angle.inc arc.inc circle.inc colours.inc copy.inc cordic.inc disc2.inc disc.inc dither.inc fan.inc graphics.inc hit.inc line.inc lines.inc pack.inc palette.inc rectangle.inc spectrum.inc wallpaper.inc

examples = \
	robot \
	robot2 \
	robot3 \
	particles-ats

all-local: $(examples)

robot: robot.occ graphics.tce
	$(OCCBUILD) --program $< graphics.tce

robot2: robot2.occ graphics.tce
	$(OCCBUILD) --program $< graphics.tce

robot3: robot3.occ graphics.tce
	$(OCCBUILD) --program $< graphics.tce

particles-ats: particles-ats.occ
	$(OCCBUILD) --program $<

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples robots --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples) graphics.tce

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
