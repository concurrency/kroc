@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	time.occ \
	time.inc

time_lib_objs = \
	time.tce

time_libs = time.lib
time_includes = $(srcdir)/time.inc
libs = $(time_libs)
includes = $(time_includes)

all-local: $(libs)

time.lib: $(time_lib_objs)
	$(OCCBUILD) --library time.lib --include time.inc $(time_lib_objs)

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(time_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

time.tce: time.inc
