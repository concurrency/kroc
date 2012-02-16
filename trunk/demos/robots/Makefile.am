@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	fan.inc \
	hit.inc \
	particles-ats.occ \
	robot.occ \
	robot2.occ \
	robot3.occ

if BUILD_ROBOTS
examples = \
	particles-ats \
	robot \
	robot2 \
	robot3
endif

robotsdir = $(prefix)/share/kroc/robots
dist_robots_DATA = \
	robot-track-1.png \
	robot-track-2.png

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples robots --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

.occ:
	$(OCCBUILD) --program $<
