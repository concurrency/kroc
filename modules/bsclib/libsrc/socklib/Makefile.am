## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@
OCCAM_DEFS = @OCCAM_DEFS@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	csock.c \
	socklib.inc \
	socklib.occ

sock_lib_objs = \
	csock.o \
	socklib.tce

all-local: sock.lib

sock.lib: $(sock_lib_objs)
	$(OCCBUILD) --library sock.lib --include socklib.inc @SOCK_LIBS@ $(sock_lib_objs)

install-exec-local: sock.lib $(srcdir)/socklib.inc
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" sock.lib $(srcdir)/socklib.inc

clean-local:
	$(OCCBUILD) --clean sock.lib $(sock_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) $(OCCAM_DEFS) --object $<
