## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
OCCAM_DEFS = @OCCAM_DEFS@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	ciftrace.h \
	ponyanstcpip.occ \
	ponyint.inc \
	ponyint1.occ \
	ponyint2.occ \
	ponyint3.occ \
	ponyintaux.occ \
	ponyintcif.c \
	ponyintcif.inc \
	ponyinttcpip.inc \
	ponyinttcpip.occ \
	ponyinttcpipsock.occ \
	ponyinttcpipsockc.c \
	ponyinttcpipstartup.occ \
	ponylib.inc \
	ponylib.occ \
	ponylibtcpip.inc \
	prot-conv-spec.txt

if BUILD_PONY
pony_lib_objs = \
	ponyint1.tce \
	ponyint2.tce \
	ponyint3.tce \
	ponyintaux.tce \
	ponyintcif.o \
	ponyinttcpip.tce \
	ponyinttcpipsock.tce \
	ponyinttcpipsockc.o \
	ponyinttcpipstartup.tce \
	ponylib.tce

ponyanstcpip_objs = \
	ponyintaux.tce \
	ponyinttcpipsock.tce \
	ponyinttcpipsockc.o

pony_libs = pony.lib
pony_includes = \
	$(srcdir)/ponyint.inc \
	$(srcdir)/ponyintcif.inc \
	$(srcdir)/ponyinttcpip.inc \
	$(srcdir)/ponylib.inc \
	$(srcdir)/ponylibtcpip.inc
pony_tools = ponyanstcpip
endif

libs = $(pony_libs)
includes = $(pony_includes)
tools = $(pony_tools)

all-local: $(libs) $(includes) $(tools)

pony.lib: $(pony_lib_objs)
	$(OCCBUILD) --library pony.lib --need sock --need file --need proc --need cif --include ponylib.inc --include ponylibtcpip.inc --include ponyint.inc --include ponyinttcpip.inc $(pony_lib_objs)

ponyanstcpip: ponyanstcpip.occ $(ponyanstcpip_objs)
	$(OCCBUILD) --program $(srcdir)/ponyanstcpip.occ $(ponyanstcpip_objs)

install-exec-local: $(libs) $(includes) $(tools)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes) $(tools)

clean-local:
	$(OCCBUILD) --clean $(libs) $(pony_lib_objs) $(tools) $(ponyanstcpip_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) $(OCCAM_DEFS) --object $<

ponyintcif.o: ciftrace.h
## begin pideps dependencies
$(srcdir)/ponylib.inc: $(srcdir)/ponylibtcpip.inc
ponyanstcpip.tce: $(srcdir)/ponyinttcpip.inc
ponyanstcpip.tce: $(srcdir)/ponylib.inc
ponyanstcpip.tce: ponyintaux.tce
ponyanstcpip.tce: ponyinttcpipsock.tce
ponyint1.tce: $(srcdir)/ponyint.inc
ponyint1.tce: $(srcdir)/ponylib.inc
ponyint1.tce: ponyintaux.tce
ponyint2.tce: $(srcdir)/ponyint.inc
ponyint2.tce: $(srcdir)/ponylib.inc
ponyint2.tce: ponyint1.tce
ponyint3.tce: $(srcdir)/ponyint.inc
ponyint3.tce: $(srcdir)/ponylib.inc
ponyint3.tce: ponyint1.tce
ponyint3.tce: ponyint2.tce
ponyint3.tce: ponyintaux.tce
ponyint3.tce: ponyinttcpipstartup.tce
ponyinttcpip.tce: $(srcdir)/ponyint.inc
ponyinttcpip.tce: $(srcdir)/ponyinttcpip.inc
ponyinttcpip.tce: $(srcdir)/ponylib.inc
ponyinttcpip.tce: ponyinttcpipsock.tce
ponyinttcpipstartup.tce: $(srcdir)/ponyint.inc
ponyinttcpipstartup.tce: $(srcdir)/ponyinttcpip.inc
ponyinttcpipstartup.tce: $(srcdir)/ponylib.inc
ponyinttcpipstartup.tce: ponyintaux.tce
ponyinttcpipstartup.tce: ponyinttcpip.tce
ponyinttcpipstartup.tce: ponyinttcpipsock.tce
ponylib.tce: $(srcdir)/ponyint.inc
ponylib.tce: $(srcdir)/ponyintcif.inc
ponylib.tce: $(srcdir)/ponylib.inc
ponylib.tce: ponyint3.tce
## end pideps dependencies
