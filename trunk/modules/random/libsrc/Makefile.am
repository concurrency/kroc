@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	random.inc \
	random.occ

random_lib_objs = \
	random.tce

random_libs = random.lib
random_includes = $(srcdir)/random.inc
libs = $(random_libs)
includes = $(random_includes)

all-local: $(libs)

random.lib: $(random_lib_objs)
	$(OCCBUILD) --library random.lib --need maths --include random.inc $(random_lib_objs)

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(random_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<
