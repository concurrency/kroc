@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	array_surface.occ \
	init.occ \
	joystick.occ \
	joy2.occ \
	sdl_info.occ \
	simple_surface.occ \
	sound_test.occ

if BUILD_OCCSDL_EXAMPLES
occSDL_examples = \
	array_surface \
	init \
	joystick \
	joy2 \
	sdl_info \
	simple_surface
endif

if BUILD_OCCSDLSOUND_EXAMPLES
occSDLsound_examples = \
	sound_test
endif

examples = $(occSDL_examples) $(occSDLsound_examples)

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples occSDL --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

.occ:
	$(OCCBUILD) --program $<
