## Process this file with automake to produce Makefile.in

@SET_MAKE@

AUTOMAKE_OPTIONS = foreign no-dependencies

OCCBUILD = @OCCBUILD@
CFLAGS = @CFLAGS@ @DEFS@ @OCCBUILD_CFLAGS@

EXTRA_DIST = \
	README \
	udc.inc \
	udc_bufchan.c \
	udc_funcs.occ \
	udc_sockchan.c \
	udc_softchan.c

udc_lib_objs = \
	udc_bufchan.o \
	udc_funcs.tce \
	udc_sockchan.o \
	udc_softchan.o

all-local: udc.lib

udc.lib: $(udc_lib_objs)
	$(OCCBUILD) --library udc.lib --include udc.inc $(udc_lib_objs)

install-exec-local: udc.lib udc.inc
	$(OCCBUILD) --install --prefix "$(prefix)" --destdir "$(DESTDIR)" udc.lib udc.inc

clean-local:
	$(OCCBUILD) --clean udc.lib $(udc_lib_objs)

SUFFIXES = .occ .tce .lib

.c.o:
	$(CC) $(CFLAGS) -c $<

.occ.tce:
	$(OCCBUILD) --object $<
