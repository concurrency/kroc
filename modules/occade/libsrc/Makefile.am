@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@ -DOCCADE.DATA.DIR="$(datadir)/occade"

EXTRA_DIST = \
	occade.occ \
	occade.inc \
	overwriting-buffer.inc

occade_lib_objs = \
	occade.tce

libs = occade.lib
includes = occade.inc overwriting-buffer.inc

all-local: $(libs)

occade.lib: $(occade_lib_objs)
	$(OCCBUILD) --library occade.lib --need occSDL --need file --need course --need raster --need rasterio --need sdlraster --include occade.inc $(occade_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean occade.lib $(occade_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
occade.occ: $(srcdir)/occade.inc
occade.occ: $(srcdir)/overwriting-buffer.inc
## end pideps dependencies
