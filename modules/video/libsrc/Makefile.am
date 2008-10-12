## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	video.inc \
	video.occ \
	video-v4l.c

video_lib_objs = \
	video.tce \
	video-v4l.o

video_libs = video.lib
video_includes = video.inc

libs = $(video_libs)
includes = $(video_includes)

all-local: $(libs) $(includes)

video.lib: $(video_lib_objs)
	$(OCCBUILD) --library video.lib --need raster --include video.inc $(video_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(video_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

#{{{  dependencies
#}}}
