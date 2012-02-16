## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@ --search ../filelib
OCCAM_DEFS = @OCCAM_DEFS@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	cproc.c \
	proclib.inc \
	proclib.occ

proc_lib_objs = \
	cproc.o \
	proclib.tce

all-local: proc.lib

proc.lib: $(proc_lib_objs)
	$(OCCBUILD) --library proc.lib --need file --include proclib.inc $(proc_lib_objs)

install-exec-local: proc.lib $(srcdir)/proclib.inc
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" proc.lib $(srcdir)/proclib.inc

clean-local:
	$(OCCBUILD) --clean proc.lib $(proc_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) $(OCCAM_DEFS) --object $<
