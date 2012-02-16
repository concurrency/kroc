## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies
ACLOCAL_AMFLAGS = -I ../../../m4

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	README \
	udc.inc \
	udc_bufchan.c \
	udc_funcs.occ \
	udc_sockchan.c \
	udc_softchan.c

if BUILD_UDC
udc_lib_objs = \
	udc_bufchan.o \
	udc_funcs.tce \
	udc_sockchan.o \
	udc_softchan.o

udc_libs = udc.lib
udc_includes = $(srcdir)/udc.inc
endif

libs = $(udc_libs)
includes = $(udc_includes)

all-local: $(libs) $(includes)

udc.lib: $(udc_lib_objs)
	$(OCCBUILD) --library udc.lib --include udc.inc $(udc_lib_objs)

install-exec-local: $(libs) $(includes)
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" $(libs) $(includes)

clean-local:
	$(OCCBUILD) --clean $(libs) $(udc_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<
