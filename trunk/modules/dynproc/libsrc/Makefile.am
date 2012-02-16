@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@

if BUILD_DYNPROC
libs = dynproc.lib
includes = $(srcdir)/dynproc.inc
endif

EXTRA_DIST = \
	$(includes)

all-local: $(libs)

dynproc.lib:
	$(OCCBUILD) --library dynproc.lib --include dynproc.inc

install-exec-local: $(libs)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs)

SUFFIXES = .lib .inc
