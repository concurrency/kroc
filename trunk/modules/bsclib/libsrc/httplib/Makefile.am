## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@ --search ../socklib --search ../filelib
OCCAM_DEFS = @OCCAM_DEFS@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	httplib.inc \
	httplib.occ

http_lib_objs = \
	httplib.tce

all-local: http.lib

http.lib: $(http_lib_objs)
	$(OCCBUILD) --library http.lib --need sock --need file --include httplib.inc $(http_lib_objs)

install-exec-local: http.lib $(srcdir)/httplib.inc
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" http.lib $(srcdir)/httplib.inc

clean-local:
	$(OCCBUILD) --clean http.lib $(http_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) $(OCCAM_DEFS) --object $<
