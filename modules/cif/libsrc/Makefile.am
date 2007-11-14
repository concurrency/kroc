@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	README \
	cif.h \
	cif.inc

libs = cif.lib
includes = cif.inc
cincludes = cif.h

all-local: $(libs) $(includes)

cif.lib:
	$(OCCBUILD) --library cif.lib --include cif.inc

install-exec-local: $(libs) $(includes) $(cincludes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes) $(cincludes)

clean-local:
	$(OCCBUILD) --clean $(libs)

SUFFIXES = .occ .tce .lib
