@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	p3dx.occ \
	pioneer.inc

pioneer_lib_objs = \
	p3dx.tce

pioneer_libs = pioneer.lib
pioneer_includes = pioneer.inc
libs = $(pioneer_libs)
includes = $(pioneer_includes)

all-local: $(libs)

pioneer.lib: $(pioneer_lib_objs)
	$(OCCBUILD) --library pioneer.lib --include pioneer.inc --need ioport $(pioneer_lib_objs)

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(pioneer_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<
