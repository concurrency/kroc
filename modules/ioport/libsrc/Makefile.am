@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	serial-c.c \
	serial.occ

ioport_lib_objs = \
	serial-c.o \
	serial.tce

ioport_libs = ioport.lib
ioport_includes = ioport.inc
libs = $(ioport_libs)
includes = $(ioport_includes)

all-local: $(libs)

ioport.lib: $(ioport_lib_objs)
	$(OCCBUILD) --library ioport.lib --include ioport.inc --need file $(ioport_lib_objs)

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(ioport_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<
