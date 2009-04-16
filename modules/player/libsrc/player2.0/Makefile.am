@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

occlib_DATA = occ_player.so occ_player.inc

SWIG_SOURCE_FILES = \
	occamplayer.i \
	player.i \
	playerc.i

SWIG_OUTPUT_FILES = \
	player_wrap.c player_wrap.h occ_player.inc

EXTRA_DIST = $(SWIG_SOURCE_FILES) $(SWIG_OUTPUT_FILES)

occplayer_libs = occ_player.lib
occplayer_includes = occ_player.inc
libs = $(occGL_libs)
includes = $(occGL_includes)

all-local: $(libs) $(includes)

if HAVE_SWIG_OCCAM
player_wrap.c player_wrap.h occplayer.inc: $(SWIG_SOURCE_FILES)
	$(SWIG_OCCAM) player.i
endif

occplayer.lib: $(occplayer_lib_objs)
	$(OCCBUILD) --library occplayer.lib --include occplayer.inc  $(occplayer_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(occplayer_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

