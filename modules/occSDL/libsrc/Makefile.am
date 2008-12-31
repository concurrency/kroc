@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

SWIG_SOURCE_FILES = \
	SDL.i \
	SDL_audio.i \
	SDL_error.i \
	SDL_events.i \
	SDL_joystick.i \
	SDL_keyboard.i \
	SDL_keysym.i \
	SDL_main.i \
	SDL_sound.i \
	SDL_types.i \
	SDL_video.i

SWIG_OUTPUT_FILES = \
	SDL_wrap.c SDL_wrap.h occSDL.inc \
	SDL_sound_wrap.c SDL_sound_wrap.h occSDLsound.inc

EXTRA_DIST = \
	$(SWIG_SOURCE_FILES) \
	$(SWIG_OUTPUT_FILES) \
	occSDLhelpers-c.c \
	occSDLhelpers.inc \
	occSDLhelpers.occ \
	occSDLjoy.inc \
	occSDLjoy.occ \
	cocoa_helper.h \
	cocoa_helper.m

occSDL_lib_objs = \
	SDL_wrap.o \
	occSDLhelpers-c.o \
	occSDLhelpers.tce \
	occSDLjoy.tce
if COCOA_HELPER
occSDL_lib_objs += cocoa_helper.o
endif

occSDLsound_lib_objs = \
	SDL_sound_wrap.o

occSDL_libs = occSDL.lib
occSDL_includes = occSDL.inc occSDLhelpers.inc occSDLjoy.inc
if HAVE_SDL_SOUND
occSDLsound_libs = occSDLsound.lib
occSDLsound_includes = occSDLsound.inc
endif
libs = $(occSDL_libs) $(occSDLsound_libs)
includes = $(occSDL_includes) $(occSDLsound_includes)

all-local: $(libs) $(includes)

if HAVE_SWIG_OCCAM
SDL_wrap.c SDL_wrap.h occSDL.inc: $(SWIG_SOURCE_FILES)
	$(SWIG_OCCAM) -structpointers SDL.i
endif

occSDL.lib: $(occSDL_lib_objs)
	$(OCCBUILD) --library occSDL.lib --include occSDL.inc --include occSDLhelpers.inc --include occSDLjoy.inc @SDL_LIBS@ $(occSDL_lib_objs)

if HAVE_SWIG_OCCAM
SDL_sound_wrap.c SDL_sound_wrap.h occSDLsound.inc: $(SWIG_SOURCE_FILES)
	$(SWIG_OCCAM) -structpointers SDL_sound.i
endif

occSDLsound.lib: $(occSDLsound_lib_objs)
	$(OCCBUILD) --library occSDLsound.lib --include occSDLsound.inc @SDL_LIBS@ -lSDL_sound $(occSDLsound_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(occSDL_lib_objs) $(occSDLsound_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.m.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

