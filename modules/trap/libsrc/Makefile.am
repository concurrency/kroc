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
trap_lib_objs = \
	selector-c.o \
	selector.tce \
	trap.tce

trap_libs = trap.lib
trap_includes = \
	selector.inc \
	trap.inc
trap_tools = trapns
endif

libs = $(trap_libs)
includes = $(trap_includes)
tools = $(trap_tools)

all-local: $(libs) $(includes) $(tools)

trap.lib: $(trap_lib_objs)
	$(OCCBUILD) --library trap.lib --need cif --need file --need sock --need useful --include trap.inc --include selector.inc $(trap_lib_objs)

trapns: trapns.occ trap.lib
	$(OCCBUILD) --program trapns.occ

install-exec-local: $(libs) $(includes) $(tools)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes) $(tools)

clean-local:
	$(OCCBUILD) --clean $(libs) $(trap_lib_objs) $(tools) $(trapanstcpip_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<

#{{{ dependencies
selector.tce: selector.inc
trap.tce: selector.inc
trap.tce: selector.tce
trap.tce: trap.inc
#}}}
