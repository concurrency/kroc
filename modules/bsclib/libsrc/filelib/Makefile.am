## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@
OCCAM_DEFS = @OCCAM_DEFS@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	cfile.c \
	filelib.inc \
	filelib.occ

file_lib_objs = \
	cfile.o \
	filelib.tce

all-local: file.lib

file.lib: $(file_lib_objs)
	$(OCCBUILD) --library file.lib --include filelib.inc $(file_lib_objs)

install-exec-local: file.lib $(srcdir)/filelib.inc
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" file.lib $(srcdir)/filelib.inc

clean-local:
	$(OCCBUILD) --clean file.lib $(file_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) $(OCCAM_DEFS) --object $<
