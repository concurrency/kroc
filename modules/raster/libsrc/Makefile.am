## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

if BUILD_RASTER
SUBDIRS = base graphics text
if BUILD_RASTERIO
SUBDIRS += io
endif
endif

