@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

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
	$(OCCBUILD) --program $<

breakout: breakout.occ shutdown.tce
	$(OCCBUILD) --program $< shutdown.tce

collision-test: collision-test.occ
	$(OCCBUILD) --program $<

parrot_attack: parrot_attack.occ shutdown.tce
	$(OCCBUILD) --program $< shutdown.tce

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples occade --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples) shutdown.tce

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
