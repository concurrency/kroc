## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	selector-c.c \
	selector.inc \
	selector.occ \
	trap.inc \
	trapns.occ \
	trap.occ

if BUILD_TRAP
selector_lib_objs = \
	selector-c.o \
	selector.tce

trap_lib_objs = \
	trap.tce

trap_libs = selector.lib trap.lib
trap_includes = \
	$(srcdir)/selector.inc \
	$(srcdir)/trap.inc
trap_tools = trapns
endif

libs = $(trap_libs)
includes = $(trap_includes)
tools = $(trap_tools)

all-local: $(libs) $(includes) $(tools)

selector.lib: $(selector_lib_objs)
	$(OCCBUILD) --library selector.lib --include selector.inc $(selector_lib_objs)

trap.lib: $(trap_lib_objs)
	$(OCCBUILD) --library trap.lib --need selector --need file --need sock --need useful --include trap.inc $(trap_lib_objs)

trapns: trapns.occ trap.lib
	$(OCCBUILD) --program $(srcdir)/trapns.occ

install-exec-local: $(libs) $(includes) $(tools)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes) $(tools)

clean-local:
	$(OCCBUILD) --clean $(libs) $(selector_lib_objs) $(trap_lib_objs) $(tools)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

#{{{ dependencies
selector.tce: $(srcdir)/selector.inc
trap.tce: selector.lib
trap.tce: $(srcdir)/trap.inc
#}}}
