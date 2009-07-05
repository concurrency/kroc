@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

SUBDIRS = worlds

EXTRA_DIST = \
	blobfinder.occ \
	hudtest.occ \
	properties.occ \
	simple.occ \
	startrobot.occ

if BUILD_PLAYER_EXAMPLES
player_examples = \
	blobfinder \
	hudtest \
	properties \
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
