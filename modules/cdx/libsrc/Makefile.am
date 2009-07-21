@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @SDL_CFLAGS@

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

if BUILD_CDX
cdx_lib_objs = \
	cdx.tce \
	cdxclient.o \
	cdxoccam.o

cdx_libs = cdx.lib
cdx_includes = $(srcdir)/cdxclient.h
cdx_programs = cdxview
cdxview_LDADD = @SDL_LIBS@ @SOCK_LIBS@

netbar_lib_objs = \
	netbar.tce

netbar_libs = netbar.lib
netbar_includes = $(srcdir)/netbar.inc
netbar_programs = netbard
netbard_LDADD = @SOCK_LIBS@
endif

bin_PROGRAMS = $(cdx_programs) $(netbar_programs)
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

