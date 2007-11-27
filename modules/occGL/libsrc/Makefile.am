@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

SWIG_SOURCE_FILES = \
	gl.i \
	glu.i

SWIG_OUTPUT_FILES = \
	opengl_wrap.c opengl_wrap.h occGL.inc

EXTRA_DIST = \
	$(SWIG_SOURCE_FILES) \
	$(SWIG_OUTPUT_FILES)

occGL_lib_objs = \
	opengl_wrap.o

occGL_libs = occGL.lib
occGL_includes = occGL.inc
libs = $(occGL_libs)
includes = $(occGL_includes)

all-local: $(libs) $(includes)

if HAVE_SWIG_OCCAM
GL_wrap.c GL_wrap.h occGL.inc: $(SWIG_SOURCE_FILES)
	$(SWIG_OCCAM) opengl.i
endif

occGL.lib: $(occGL_lib_objs)
	$(OCCBUILD) --library occGL.lib --include occGL.inc @GL_LIBS@ $(occGL_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(occGL_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

