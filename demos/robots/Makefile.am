@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	angle.inc \
	arc.inc \
	circle.inc \
	colours.inc \
	copy.inc \
	cordic.inc \
	disc.inc \
	disc2.inc \
	dither.inc \
	fan.inc \
	graphics.inc \
	graphics.occ \
	hit.inc \
	line.inc \
	lines.inc \
	pack.inc \
	palette.inc \
	particles-ats.occ \
	rectangle.inc \
	robot.occ \
	robot2.occ \
	robot3.occ \
	spectrum.inc \
	wallpaper.inc

examples = \
	robot \
	robot2 \
	robot3 \
	particles-ats

robotsdir = $(prefix)/share/kroc/robots
dist_robots_DATA = \
	robot-track-1.png \
	robot-track-2.png

all-local: $(examples)

robot: robot.occ graphics.tce
	$(OCCBUILD) --program robot.occ graphics.tce

robot2: robot2.occ graphics.tce
	$(OCCBUILD) --program robot2.occ graphics.tce

robot3: robot3.occ graphics.tce
	$(OCCBUILD) --program robot3.occ graphics.tce

particles-ats: particles-ats.occ
	$(OCCBUILD) --program particles-ats.occ

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples robots --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples) graphics.tce

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
