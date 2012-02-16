## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@ --kroc-opts -d

EXTRA_DIST = \
	ttyutil.occ \
	ttyutil.inc

if BUILD_TTYUTIL
ttyutil_lib_objs = \
	ttyutil.tce

libs = ttyutil.lib
includes = $(srcdir)/ttyutil.inc
endif

all-local: $(libs)

ttyutil.lib: $(ttyutil_lib_objs)
	$(OCCBUILD) --library ttyutil.lib --need course --include ttyutil.inc $(ttyutil_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean ttyutil.lib $(ttyutil_lib_objs)

SUFFIXES = .occ .tce .lib

.occ.tce:
	$(OCCBUILD) --object $<

#{{{  dependencies
#}}}
