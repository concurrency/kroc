## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	video.inc \
	video.occ \
	video-v4l.c \
	video-v4l.h \
	ccvt.h \
	ccvt_types.h \
	ccvt_misc.c \
	vtest.occ

tests = \
	vtest

video_lib_objs = \
	video.tce \
	video-v4l.o \
	ccvt_misc.o

video_libs = video.lib
video_includes = video.inc

libs = $(video_libs)
includes = $(video_includes)

all-local: $(libs) $(includes) $(tests)

video.lib: $(video_lib_objs)
	$(OCCBUILD) --library video.lib --need raster --include video.inc $(video_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(video_lib_objs) $(tests)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

.occ:
	$(OCCBUILD) --program $<

#{{{  dependencies
#}}}
