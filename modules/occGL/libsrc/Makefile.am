@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @CPPFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

SWIG_SOURCE_FILES = \
	gl.i \
	glu.i \
	opengl.i

SWIG_OUTPUT_FILES = \
	opengl_wrap.c opengl_wrap.h occGL.inc

EXTRA_DIST = \
	$(SWIG_SOURCE_FILES) \
	$(SWIG_OUTPUT_FILES)

if BUILD_OCCGL
occGL_lib_objs = \
	opengl_wrap.o

occGL_libs = occGL.lib
occGL_includes = $(srcdir)/occGL.inc
endif

libs = $(occGL_libs)
includes = $(occGL_includes)

all-local: $(libs) $(includes)

if HAVE_SWIG_OCCAM
$(srcdir)/GL_wrap.c $(srcdir)/GL_wrap.h $(srcdir)/occGL.inc: $(SWIG_SOURCE_FILES)
	cd $(srcdir) && $(SWIG_OCCAM) opengl.i
endif

occGL.lib: $(occGL_lib_objs)
	$(OCCBUILD) --library occGL.lib --include occGL.inc @LDFLAGS@ @GL_LIBS@ $(occGL_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(occGL_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

