## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@ --search ../base
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	angle.inc \
	arc.occ \
	circle.occ \
	colourspace.occ \
	copy.occ \
	cordic.occ \
	disc.occ \
	disc2.occ \
	dither.occ \
	ellipse.occ \
	graphics.inc \
	hit.occ \
	line.occ \
	lines.occ \
	rectangle.occ \
	spectrum.occ \
	wallpaper.occ

rastergraphics_lib_objs = \
	arc.tce \
	circle.tce \
	colourspace.tce \
	copy.tce \
	cordic.tce \
	disc.tce \
	disc2.tce \
	dither.tce \
	ellipse.tce \
	hit.tce \
	line.tce \
	lines.tce \
	rectangle.tce \
	spectrum.tce \
	wallpaper.tce

rastergraphics_libs = rastergraphics.lib
rastergraphics_includes = \
	$(srcdir)/angle.inc \
	$(srcdir)/graphics.inc
libs = $(rastergraphics_libs)
includes = $(rastergraphics_includes)

all-local: $(libs) $(includes)

rastergraphics.lib: $(rastergraphics_lib_objs)  
	$(OCCBUILD) --library rastergraphics.lib --need raster --include angle.inc --include graphics.inc $(rastergraphics_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(rastergraphics_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<


## begin pideps dependencies
arc.tce: cordic.tce
cordic.tce: $(srcdir)/angle.inc
spectrum.tce: $(srcdir)/graphics.inc
## end pideps dependencies
