@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

EXTRA_DIST = \
	README \
	cif.h \
	cif.inc

if BUILD_CIF
libs = cif.lib
includes = $(srcdir)/cif.inc
cincludes = $(srcdir)/cif.h
endif

all-local: $(libs) $(includes)

cif.lib:
	$(OCCBUILD) --library cif.lib --include cif.inc

install-exec-local: $(libs) $(includes) $(cincludes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes) $(cincludes)

clean-local:
	$(OCCBUILD) --clean $(libs)

SUFFIXES = .occ .tce .lib
