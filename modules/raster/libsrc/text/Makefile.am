## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@ --search ../base
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	convert-font \
	convert-varfont \
	font10x20.inc \
	font6x13.inc \
	text.occ

rastertext_lib_objs = \
	text.tce

rastertext_libs = rastertext.lib
rastertext_includes = \
	$(srcdir)/font10x20.inc \
	$(srcdir)/font6x13.inc
libs = $(rastertext_libs)
includes = $(rastertext_includes)

all-local: $(libs) $(includes)

rastertext.lib: $(rastertext_lib_objs)
	$(OCCBUILD) --library rastertext.lib --need raster --include font10x20.inc --include font6x13.inc $(rastertext_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(rastertext_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<
