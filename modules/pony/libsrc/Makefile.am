## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

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
	ponylibtcpip.inc

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

all-local: pony.lib ponyanstcpip

pony.lib: $(pony_lib_objs)
	$(OCCBUILD) --library pony.lib --need sock --need file --need proc --need cif --include ponylib.inc --include ponylibtcpip.inc --include ponyint.inc --include ponyinttcpip.inc $(pony_lib_objs)

ponyanstcpip: ponyanstcpip.occ $(ponyanstcpip_objs)
	$(OCCBUILD) --program ponyanstcpip.occ $(ponyanstcpip_objs)

install-exec-local: pony.lib ponylib.inc ponylibtcpip.inc ponyanstcpip
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" pony.lib ponylib.inc ponylibtcpip.inc ponyint.inc ponyinttcpip.inc ponyanstcpip

clean-local:
	$(OCCBUILD) --clean pony.lib $(pony_lib_objs) ponyanstcpip $(ponyanstcpip_objs)

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
