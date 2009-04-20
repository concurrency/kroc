@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	simple.occ \
	startrobot.occ

if BUILD_PLAYER_EXAMPLES
player_examples = \
	simple \
	startrobot
endif

examples = $(player_examples)

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples player --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
