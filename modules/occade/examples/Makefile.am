@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

SUBDIRS = images

EXTRA_DIST = \
	bounce.occ \
	breakout.occ \
	collision-test.occ \
	flipbook.occ \
	parrot_attack.occ \
	scrollytext.occ \
	shutdown.inc \
	shutdown.occ

if BUILD_OCCADE_EXAMPLES
examples = \
	bounce \
	breakout \
	collision-test \
	flipbook \
	parrot_attack \
	scrollytext
endif

all-local: $(examples)

bounce: bounce.occ
	$(OCCBUILD) --program $(srcdir)/bounce.occ

breakout: breakout.occ shutdown.tce
	$(OCCBUILD) --program $(srcdir)/breakout.occ shutdown.tce

collision-test: collision-test.occ
	$(OCCBUILD) --program $(srcdir)/collision-test.occ

flipbook: flipbook.occ
	$(OCCBUILD) --program $(srcdir)/flipbook.occ

parrot_attack: parrot_attack.occ shutdown.tce
	$(OCCBUILD) --program $(srcdir)/parrot_attack.occ shutdown.tce

scrollytext: scrollytext.occ
	$(OCCBUILD) --program $(srcdir)/scrollytext.occ

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples occade --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples) shutdown.tce

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
