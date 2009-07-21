@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @MYSQLCLIENT_CFLAGS@

SUBDIRS = doc

EXTRA_DIST = \
	moa.inc \
	mysql.occ \
	mysql_connector.c

if BUILD_MOA
moa_lib_objs = \
	mysql.tce \
	mysql_connector.o

moa_libs = moa.lib
moa_includes = $(srcdir)/moa.inc
endif

libs = $(moa_libs)
includes = $(moa_includes)

all-local: $(libs) $(includes)

moa.lib: $(moa_lib_objs)
	$(OCCBUILD) --library moa.lib --need cif --include moa.inc @MYSQLCLIENT_LIBS@ $(moa_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(moa_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

