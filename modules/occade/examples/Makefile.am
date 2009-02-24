@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

SUBDIRS = images

EXTRA_DIST = \
	bounce.occ \
	breakout.occ \
	collision-test.occ \
	parrot_attack.occ \
	shutdown.inc \
	shutdown.occ

examples = \
	bounce \
	breakout \
	collision-test \
	parrot_attack

all-local: $(examples)

bounce: bounce.occ
	$(OCCBUILD) --program bounce.occ

breakout: breakout.occ shutdown.tce
	$(OCCBUILD) --program breakout.occ shutdown.tce

collision-test: collision-test.occ
	$(OCCBUILD) --program collision-test.occ

parrot_attack: parrot_attack.occ shutdown.tce
	$(OCCBUILD) --program parrot_attack.occ shutdown.tce

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples occade --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples) shutdown.tce

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
