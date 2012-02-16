## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@
OCCAM_DEFS = @OCCAM_DEFS@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	cspdrvc.c \
	cspdrvc.h \
	cspdrvlib.inc \
	cspdrvlib.occ \
	cspioctl.h

cspdrv_lib_objs = \
	cspdrvc.o \
	cspdrvlib.tce

all-local: cspdrv.lib

cspdrv.lib: $(cspdrv_lib_objs)
	$(OCCBUILD) --library cspdrv.lib --include cspdrvlib.inc $(cspdrv_lib_objs)

install-exec-local: cspdrv.lib $(srcdir)/cspdrvlib.inc
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" cspdrv.lib $(srcdir)/cspdrvlib.inc

clean-local:
	$(OCCBUILD) --clean cspdrv.lib $(cspdrv_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) $(OCCAM_DEFS) --object $<
