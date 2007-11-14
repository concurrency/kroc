@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	occade.occ \
	occade.inc \
	overwriting-buffer.inc

occade_lib_objs = \
	occade.tce

all-local: occade.lib

occade.lib: $(occade_lib_objs)
	$(OCCBUILD) --library occade.lib --need occSDL --need file --need course --need sdlraster --include occade.inc $(occade_lib_objs)

install-exec-local: occade.lib occade.inc
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" occade.lib occade.inc

clean-local:
	$(OCCBUILD) --clean occade.lib $(occade_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

## begin pideps dependencies
occade.tce: $(srcdir)/occade.inc
occade.tce: $(srcdir)/overwriting-buffer.inc
## end pideps dependencies
