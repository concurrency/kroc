@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	array_surface.occ \
	init.occ \
	joystick.occ \
	sdl_info.occ \
	simple_surface.occ \
	sound_test.occ

if HAVE_SDL_SOUND
sound_examples = \
	sound_test
endif
examples = \
	array_surface \
	init \
	joystick \
	sdl_info \
	simple_surface \
	$(sound_examples)

all-local: $(examples)

install-exec-local: $(examples)
	$(OCCBUILD) --install-examples occSDL --prefix "$(prefix)" --destdir "$(DESTDIR)" $(examples)

clean-local:
	$(OCCBUILD) --clean $(examples)

SUFFIXES = .occ

%: %.occ
	$(OCCBUILD) --program $<
