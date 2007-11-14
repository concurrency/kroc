@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
OCCBUILD = @OCCBUILD@

libs = dynproc.lib
includes = dynproc.inc

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
