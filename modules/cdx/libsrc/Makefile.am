@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@ @SDL_CFLAGS@

EXTRA_DIST = \
	README \
	cdx.occ \
	cdxclient.c \
	cdxclient.h \
	cdxoccam.c \
	cdxview.c \
	netbar.inc \
	netbar.occ \
	netbard.c

bin_PROGRAMS = cdxview netbard
cdxview_LDADD = @SDL_LIBS@

cdx_lib_objs = \
	cdx.tce \
	cdxclient.o \
	cdxoccam.o

netbar_lib_objs = \
	netbar.tce

cdx_libs = cdx.lib
cdx_includes = cdxclient.h
netbar_libs = netbar.lib
netbar_includes = netbar.inc
libs = $(cdx_libs) $(netbar_libs)
includes = $(cdx_includes) $(netbar_includes)

all-local: $(libs) $(includes)

cdx.lib: $(cdx_lib_objs)
	$(OCCBUILD) --library cdx.lib $(cdx_lib_objs)

netbar.lib: $(netbar_lib_objs)
	$(OCCBUILD) --library netbar.lib --need sock --include netbar.inc $(netbar_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(cdx_lib_objs) $(netbar_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

